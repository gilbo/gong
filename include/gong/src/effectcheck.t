import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.effectcheck"] = Exports

-------------------------------------------------------------------------------

local Typechecker = require 'gong.src.typechecker'
local T           = require 'gong.src.types'
local Util        = require 'gong.src.util'
local AST         = Typechecker.AST
local is_type     = T.is_type
local SrcInfo     = Util.SrcInfo
local is_id_str   = Util.is_id_str
local INTERNAL_ERR  = Util.INTERNAL_ERR

local Schemata    = require 'gong.src.schemata'
local Macro       = require 'gong.src.macro'
local Global      = require 'gong.src.global'
local Functions   = require 'gong.src.functions'
local is_table    = Schemata.is_table
local is_global   = Global.is_global

local newlist   = terralib.newlist


-------------------------------------------------------------------------------
--[[                                Effects                                ]]--
-------------------------------------------------------------------------------

local redop_token_set       = Typechecker.redop_token_set
Exports.redop_token_set     = redop_token_set

local ADT E
  
  Effect = Scan         { src  : Type }
         | Filter       {}
         | Emit         { dst  : Type } -- destination table
         | Merge        { dst  : Type }
         | MergeRemove  { dst : Type }
         | Return       { type : Type }
         | OverWrite    { dst  : Type,        path : PathToken* }
         | Write        { dst  : Type,        path : PathToken* }
         | Reduce       { op   : redop,
                          dst  : Type,        path : PathToken* }
         | Read         { src  : Type,        path : PathToken* }
         | ReduceG      { op   : redop,
                          dst  : Glob,        path : PathToken* }
         | ReadG        { src  : Glob,        path : PathToken* }
         | Print        {}
        attributes      {                     srcinfo : SrcInfo }

  PathToken = FieldToken { name : id_str }
            | IndexToken { } -- assume all of the indices are hit for now

  extern redop    function(obj) return redop_token_set[obj] end

  extern Type     is_type
  extern Glob     is_global

  extern id_str   is_id_str
  extern SrcInfo  function(obj) return SrcInfo.check(obj) end
end

Exports.Effects = E

-------------------------------------------------------------------------------
--[[                            Effect Methods                             ]]--
-------------------------------------------------------------------------------


function E.Effect:path_id()
  if not self.path then return '' end
  local ids           = newlist()
  for _,tkn in ipairs(self.path) do
    if E.FieldToken.check(tkn) then
      ids:insert(       tkn.name )
    end
  end
  return ids:concat('.')
end

-------------------------------------------------------------------------------
--[[                          Context Definition                           ]]--
-------------------------------------------------------------------------------
local Context = {}
Context.__index = Context

local function NewContext()
  local ctxt = setmetatable({
    diag          = terralib.newdiagnostics(),
    _let_depth    = 0,
    _loop_depth   = 0,
    _vardefstack  = newlist(),
    _vardefs      = terralib.newenvironment(nil),
    _mode         = nil,
    _in_remove    = false,
    _permissions  = {},
    _emit_tables  = {},
    _scan_tables  = {},
    _all_effects  = newlist(),
    _all_subfuncs = {},
  }, Context)
  return ctxt
end
function Context:error(ast, ...)
  self.diag:reporterror(ast.srcinfo, ...)
end
function Context:finishandabortiferrors(lvl)
  self.diag:finishandabortiferrors("Errors during typechecking", lvl+1)
end

function Context:add_func(f)
  self._all_subfuncs[f] = true
end
function Context:add_funcs(fs)
  for _,f in ipairs(fs) do self:add_func(f) end
end

function Context:effectlist()       return self._all_effects:copy() end
function Context:subfunclist()
  local fs            = newlist()
  for func,_ in pairs(self._all_subfuncs) do
    fs:insert(func)
  end
  return fs
end

function Context:enter_noassign()
  self._vardefstack:insert(self._vardefs)
  self._vardefs = terralib.newenvironment(nil)
end
function Context:leave_noassign()
  self._vardefs = self._vardefstack:remove()
end
function Context:definevar(v)
  self._vardefs:localenv()[v] = true
end
function Context:is_definedvar(v)
  return self._vardefs:localenv()[v]
end
function Context:in_noassign()
  return #self._vardefstack > 0
end

function Context:enterloop()      self._loop_depth = self._loop_depth+1   end
function Context:leaveloop()      self._loop_depth = self._loop_depth-1   end
function Context:in_loop()    return self._loop_depth > 0               end

function Context:enter_letblock()
  self._let_depth = self._let_depth+1
  self:enter_noassign()
end
function Context:leave_letblock()
  self:leave_noassign()
  self._let_depth = self._let_depth-1
end
function Context:enter_mergeblock_remove(varname, tbl)
  self._in_remove     = true
  self:enter_mergeblock(varname, tbl)
end
function Context:leave_mergeblock_remove()
  self:leave_mergeblock()
  self._in_remove     = false
end
function Context:enter_mergeblock(varname, tbl)
  self._merge_var     = varname
  self._merge_tbl     = tbl
  self._in_merge_flag = true
  self:enter_noassign()
  self:definevar(varname)
end
function Context:leave_mergeblock()
  self:leave_noassign()
  self._in_merge_flag = nil
  self._merge_var     = nil
  self._merge_tbl     = nil
end
function Context:enter_function()   self._mode      = 'function'          end
function Context:leave_function()   self._mode      = nil                 end
function Context:enter_joinfilter() self._mode      = 'filter'            end
function Context:leave_joinfilter() self._mode      = nil                 end
function Context:enter_joindo()     self._mode      = 'do'                end
function Context:leave_joindo()     self._mode      = nil                 end

function Context:in_letblock()
  return self._let_depth > 0
end
function Context:in_remove_merge()  return self._in_remove          end
function Context:in_mergeblock()    return self._in_merge_flag      end
function Context:in_function()      return self._mode == 'function' end
function Context:in_joinfilter()    return self._mode == 'filter'   end
function Context:in_joindo()        return self._mode == 'do'       end

function Context:get_mergevar()     return self._merge_var          end
function Context:get_mergetable()   return self._merge_tbl          end

function Context:add_effect(eff, opt)
  local err         = false
  opt               = opt or {}
  if E.Read.check(eff) then -- pretty generic
    if not self:try_read(eff, eff.src:table(), eff.path) then
      err = true end

  elseif E.ReadG.check(eff) then
    if not self:try_read(eff, eff.src, eff.path) then
      err = true end

  elseif E.Print.check(eff) then
    -- always fine

  elseif self:in_letblock() then
    err = true
    self:error(eff, "no effects are allowed inside of let-expression "..
                    "statement blocks")

  elseif self:in_mergeblock() then
    if E.Write.check(eff) or E.Reduce.check(eff) then
      -- check that the mergevar matches
      local e   = opt.mergeexpr
      if not AST.Var.check(e) or e.name ~= self:get_mergevar() then
        err = true
        self:error(eff,
          "Writing and reducing inside a merge-statement is only allowed "..
          "for the merge variable '"..tostring(self:get_mergevar()).."'")
      else
      -- check that the a merge-write field is not a primary-key
        assert(#eff.dst:table():primary_key() == 2,
               'INTERNAL: expect 2 primary keys')
        local kf1, kf2  = unpack(eff.dst:table():primary_key())
        if #eff.path == 1 and
           E.FieldToken.check(eff[1])
        then
          if (eff[1].name == kf1:name() or eff[1].name == kf2:name()) then
            err = true
            self:error(eff,
              "Cannot write to primary key fields inside a merge")
          end
        end
      end
    else
      err = true
      self:error("no effects beside writing and reducing "..
                 "the merge variable are allowed inside a merge-statement")
    end

  elseif E.Filter.check(eff) then
    if not self:in_joinfilter() then
      err = true
      self:error(eff,
        "can only use where statements inside a join, before the do")
    end

  elseif E.Return.check(eff) then
    if not self:in_function() then
      err = true
      self:error(eff,
        "return statements are only allowed inside functions")
    end

  elseif E.Emit.check(eff) then
    if not self:in_joindo() then
      err = true
      self:error(eff,
        "emit statements are only allowed inside join do blocks")
    elseif self._scan_tables[eff.dst] then
      self:error(eff,
        "joins must emit into a different table than the operands "..
        "of the join.")
    elseif #eff.dst:table():primary_key() > 0 and
           self._emit_tables[eff.dst]
    then
      self:error(eff,
        "cannot emit or merge more than once into a table with "..
        "a primary key, because this may result in duplicates")
    else
      if not self:try_emit(eff, eff.dst:table()) then
        err = true end
      self._emit_tables[eff.dst] = eff
    end

  elseif E.Merge.check(eff) then
    if not self:in_joindo() then
      err = true
      self:error(eff,
        "merge statements are only allowed inside join do blocks")
    elseif self._scan_tables[eff.dst] then
      self:error(eff,
        "joins must merge into a different table than the operands "..
        "of the join.")
    elseif self._emit_tables[eff.dst] then
      self:error(eff,
        "cannot emit or merge more than once into a table with "..
        "a primary key, because this may result in duplicates")
    else
      if not self:try_merge(eff, eff.dst:table()) then
        err = true end
      self._emit_tables[eff.dst] = eff
    end

  elseif E.MergeRemove.check(eff) then
    -- we checked on the merge already
    assert(self._emit_tables[eff.dst], "INTERNAL")

  elseif E.Reduce.check(eff) or E.ReduceG.check(eff) then
    if not self:in_joindo() then
      err = true
      self:error(eff,
        "reductions are only allowed inside join do blocks")
    else
      if E.Reduce.check(eff) then
        if not self:try_reduce(eff, eff.dst:table(), eff.path) then
          err = true end
      elseif E.ReduceG.check(eff) then
        if not self:try_reduce(eff, eff.dst, eff.path) then
          err = true end
      else INTERNAL_ERR('impossible case') end
    end

  elseif E.Write.check(eff) then
    err = true
    self:error(eff,
      "writing to table fields is not allowed in gong outside merges")

  elseif E.Scan.check(eff) then
    self._scan_tables[eff.src] = true
    -- all good

  else
    INTERNAL_ERR("unrecognized effect type: "..tostring(eff))
  end

  -- accumulate in listing of all effects
  if not err then
    self._all_effects:insert(eff)
  end
end

local TERMINAL        = {}
function Context:_INTERNAL_gettbl(tbl)
  local p             = self._permissions[tbl]
  if p then return p end

  -- if we couldn't find the table, then
  -- do a full setup pass for it
  p                   = {}
  self._permissions[tbl] = p

  local function expand_field(fp, typ)
    if typ:is_tensor() then
      expand_field(fp, typ:basetype())
    elseif typ:is_record() then
      for _,f in ipairs(typ.fields) do
        fp[f.name]    = {}
        expand_field(fp[f.name], f.type)
      end
    else
      fp[TERMINAL]    = true
    end
  end

  if is_table(tbl) then
    for _,f in ipairs(tbl:fields()) do
      p[f:name()]       = {}
      expand_field(p[f:name()], f:type())
    end
  elseif is_global(tbl) then
    expand_field(p, tbl:type())
  else INTERNAL_ERR('impossible case') end

  return p
end
function Context:_INTERNAL_getpath(tbl, path)
  local p = self:_INTERNAL_gettbl(tbl)

  -- Now collect a list of all terminal fields under the given path
  local terminals     = newlist()
  local function gather_terms(fp, i)
    local tkn         = path[i] or nil
    if tkn then
      if tkn.name then
        gather_terms( assert(fp[tkn.name]), i+1 )
      else
        gather_terms( fp, i+1 )
      end
    elseif fp[TERMINAL] then
      terminals:insert(fp)
    else
      for k,subp in pairs(fp) do
        gather_terms( subp, i )
      end
    end
  end
  gather_terms(p, 1)

  return terminals
end

local function access_template(self,eff,tbl,path, new_mode)
  local ctxt    = self
  local fs      = self:_INTERNAL_getpath(tbl, path)
  local haserr  = false
  local function err(msg)
    if haserr then return end -- cut down on reporting volume
    haserr      = true
    local path  = tbl:name() .. ((#path > 0) and ('.'..eff:path_id()) or '')
    ctxt:error(eff, "cannot "..new_mode.." "..path.." "..
                    "without conflicting with "..msg)
  end
  for _,f in ipairs(fs) do
    if f.mode and f.mode == 'merge' then
      if not self:in_mergeblock() or not self:get_mergetable() == tbl then
        err("merge at "..tostring(f.eff.srcinfo))
      elseif new_mode == 'read' or new_mode:sub(1,6)=='reduce' or
             new_mode == 'write'
      then -- all-good, leave as merge
      else
        err("merge at "..tostring(f.eff.srcinfo))
      end
    elseif f.mode and f.mode ~= new_mode then
      err(f.mode.." at "..tostring(f.eff.srcinfo))
    else
      f.mode = new_mode; f.eff = eff
    end
  end

  return not haserr
end
function Context:try_read(eff, tbl, path)
  return access_template(self, eff, tbl, path, 'read')
end
function Context:try_reduce(eff, tbl, path)
  return access_template(self, eff, tbl, path, 'reduce('..eff.op..')')
end
function Context:try_write(eff, tbl, path)
  return access_template(self, eff, tbl, path, 'write')
end
function Context:try_emit(eff, tbl)
  return access_template(self, eff, tbl, newlist(), 'emit')
end
function Context:try_merge(eff, tbl)
  return access_template(self, eff, tbl, newlist(), 'merge')
end


-------------------------------------------------------------------------------
--[[                              Entry Point                              ]]--
-------------------------------------------------------------------------------

function Exports.effectcheck(input_ast)
  local ctxt        = NewContext()

  local effects     = input_ast:effectcheck(ctxt)
  local subfuncs    = ctxt:subfunclist()
  ctxt:finishandabortiferrors(3)

  return effects, subfuncs
end

local function effectcheck_all(xs, ctxt)
  return xs:map(function(x) return x:effectcheck(ctxt) end)
end


-------------------------------------------------------------------------------
--[[                            Top-Level Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Function:effectcheck(ctxt)
  effectcheck_all(self.args, ctxt)

  ctxt:enter_function()
  self.body:effectcheck(ctxt)
  ctxt:leave_function()

  if not ctxt.return_flag then
    ctxt:error(self, "expected function to return a value, but was unable "..
                     "to guarantee that")
  end

  -- gather effects and add to function object, excluding return effects
  local effs          = ctxt:effectlist():filter(function(e)
                          return not E.Return.check(e)
                        end)
  return effs
end

function AST.Join:effectcheck(ctxt)
  effectcheck_all(self.args, ctxt)

  -- add scan effects
  ctxt:add_effect( E.Scan(self.args[1].type, self.args[1].srcinfo) )
  ctxt:add_effect( E.Scan(self.args[2].type, self.args[2].srcinfo) )

  ctxt:enter_joinfilter()
  self.filter:effectcheck(ctxt)
  ctxt:leave_joinfilter()
  ctxt:enter_joindo()
  self.doblock:effectcheck(ctxt)
  ctxt:leave_joindo()

  -- gather effects and add to join object?
  return ctxt:effectlist()
end

function AST.ArgDecl:effectcheck(ctxt)
  ctxt:definevar(self.name)
end


-------------------------------------------------------------------------------
--[[                            Statement Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Block:effectcheck(ctxt)
  -- make sure returns are the end of the block
  for i,s in ipairs(self.stmts) do
    if ctxt.return_flag then
      ctxt:error(s,
        "cannot reach this statement after function has returned")
      ctxt.return_flag = false
    end
    s:effectcheck(ctxt)
  end
end

function AST.WhereFilter:effectcheck(ctxt)
  self.expr:effectcheck(ctxt)
  ctxt:add_effect(  E.Filter(self.srcinfo) )
end
function AST.EmitStmt:effectcheck(ctxt)
  if #self.dst:table():primary_key() > 0 and ctxt:in_loop() then
    ctxt:error(self,
      "cannot emit into table with primary-key multiple times, "..
      "but emit statement was inside of a loop")
  end

  self.record:effectcheck(ctxt)
  ctxt:add_effect(  E.Emit(self.dst, self.srcinfo) )
end
function AST.ReturnStmt:effectcheck(ctxt)
  self.expr:effectcheck(ctxt)
  ctxt.return_flag  = true
  ctxt:add_effect(  E.Return(self.expr.type, self.srcinfo) )
end
function AST.BreakStmt:effectcheck(ctxt)
end

function AST.MergeStmt:effectcheck(ctxt)
  if ctxt:in_mergeblock() then
    ctxt:error(self, "cannot merge inside of another merge")
  elseif ctxt:in_loop() then
    ctxt:error(self, "cannot merge multiple times, but merge statement "..
                     "was inside a loop")
  else
    -- effect
    ctxt:add_effect( E.Merge(self.dst, self.srcinfo) )

    if self.new_emit then
      self.new_emit.record:effectcheck(ctxt)
    end

    -- set guard flags and process the update body
    ctxt:enter_mergeblock( self.up_name, self.dst )
      self.up_body:effectcheck(ctxt)
    ctxt:leave_mergeblock()

    if self.rm_name then
      ctxt:add_effect( E.MergeRemove(self.dst, self.srcinfo) )
      ctxt:enter_mergeblock_remove( self.rm_name, self.dst )
        self.rm_body:effectcheck(ctxt)
      ctxt:leave_mergeblock_remove()
    end
  end
end

function AST.KeepStmt:effectcheck(ctxt)
  if ctxt:in_remove_merge() then
    local mergevar  = ctxt:get_mergevar()
    if not AST.Var.check(self.arg) or self.arg.name ~= mergevar then
      ctxt:error(self, "argument to keep() statement was not "..
                       "the argument to remove("..tostring(mergevar)..")")
    end
  else
    ctxt:error("Cannot execute keep() statement "..
               "outside of a merge-remove() block")
  end
end


local function get_readwrite_path(lval, ctxt)
  return lval.path:map(function(p)
    p:effectcheck(ctxt)
    if AST.PathField.check(p) then
      return E.FieldToken(p.name)
    elseif AST.PathIndex.check(p) then
      return E.IndexToken()
    else INTERNAL_ERR('unexpected path token') end
  end)
end
function AST.PathField:effectcheck(ctxt) end
function AST.PathIndex:effectcheck(ctxt)
  effectcheck_all(self.args, ctxt)
end
function AST.Assignment:effectcheck(ctxt)
  local lval        = self.lval
  -- check for write?
  if AST.TableWrite.check(lval) then
    local path      = get_readwrite_path(lval, ctxt)
    lval            = lval.base
    ctxt:add_effect(  E.Write( lval.type, path, self.srcinfo ),
                      { mergeexpr = lval } )
  elseif AST.GlobalWrite.check(lval) then
    ctxt:error(lval, "Assignment to globals in joins is not allowed")
  elseif ctxt:in_noassign() then
    -- check that we're not trying to assign to a variable defined
    -- outside the no-assignment scope
    while not AST.Var.check(lval) do
      if AST.TensorIndex.check(lval) then
        lval = lval.base
      elseif AST.RecordRead.check(lval) then
        lval = lval.base
      else INTERNAL_ERR("unexpected lvalue: "..tostring(lval)) end
    end
    if not ctxt:is_definedvar(lval.name) then
      ctxt:error(lval, "Cannot assign to '"..tostring(lval.name).."' "..
                       "because it was defined outside of this "..
                       "'no-assignment block' (either a "..
                       "let-expression or a merge-statement)")
    end
  end
  lval:effectcheck(ctxt)
  self.rval:effectcheck(ctxt)
end
function AST.DeclStmt:effectcheck(ctxt)
  ctxt:definevar(self.name)
  self.rval:effectcheck(ctxt)
end
function AST.Reduction:effectcheck(ctxt)
  local lval        = self.lval
  -- check for write?
  if AST.TableWrite.check(lval) then
    local path      = get_readwrite_path(lval, ctxt)
    lval            = lval.base
    ctxt:add_effect(  E.Reduce( self.op, lval.type, path, self.srcinfo ),
                      { mergeexpr = lval } )
  elseif AST.GlobalWrite.check(lval) then
    local path      = get_readwrite_path(lval, ctxt)
    ctxt:add_effect(  E.ReduceG( self.op, lval.base, path, self.srcinfo ))
    lval            = nil
  elseif ctxt:in_noassign() then
    -- check that we're not trying to reduce to a variable defined
    -- outside the no-assignment scope
    while not AST.Var.check(lval) do
      if AST.TensorIndex.check(lval) then
        lval = lval.base
      elseif AST.RecordRead.check(lval) then
        lval = lval.base
      else INTERNAL_ERR("unexpected lvalue: "..tostring(lval)) end
    end
    if not ctxt:is_definedvar(lval.name) then
      ctxt:error(lval, "Cannot reduce into '"..tostring(lval.name).."' "..
                       "because it was defined outside of this "..
                       "'no-assignment block' (either a "..
                       "let-expression or a merge-statement)")
    end
  end
  if lval then lval:effectcheck(ctxt) end
  self.rval:effectcheck(ctxt)
end

function AST.IfStmt:effectcheck(ctxt)
  local has_return    = true
  for _,c in ipairs(self.cases) do
    ctxt.return_flag  = false
    c:effectcheck(ctxt)
    if not ctxt.return_flag then has_return = false end
  end
  if self.else_body then
    ctxt.return_flag  = false
    self.else_body:effectcheck(ctxt)
    if not ctxt.return_flag then has_return = false end
  else
    has_return = false
  end
  ctxt.return_flag    = has_return
end
function AST.IfCase:effectcheck(ctxt)
  self.cond:effectcheck(ctxt)
  self.body:effectcheck(ctxt)
end
function AST.ForLoop:effectcheck(ctxt)
  ctxt:definevar(self.itername)
  self.lo:effectcheck(ctxt)
  self.hi:effectcheck(ctxt)
  if self.stride then
    self.stride:effectcheck(ctxt)
  end
  ctxt:enterloop()
  self.body:effectcheck(ctxt)
  ctxt:leaveloop()
  ctxt.return_flag  = false
end


-------------------------------------------------------------------------------
--[[                            Expression Rules                           ]]--
-------------------------------------------------------------------------------

function AST.Var:effectcheck(ctxt)
end
function AST.LuaObj:effectcheck(ctxt)
end
function AST.NumLiteral:effectcheck(ctxt)
end
function AST.BoolLiteral:effectcheck(ctxt)
end

--------------------------------------

function AST.RecordExpr:effectcheck(ctxt)
  effectcheck_all(self.exprs, ctxt)
end
function AST.ListExpr:effectcheck(ctxt)
  effectcheck_all(self.exprs, ctxt)
end

--------------------------------------

function AST.TensorIndex:effectcheck(ctxt)
  self.base:effectcheck(ctxt)
  effectcheck_all(self.args, ctxt)
end
function AST.RecordRead:effectcheck(ctxt)
  self.base:effectcheck(ctxt)
end
function AST.TableRead:effectcheck(ctxt)
  local path        = get_readwrite_path(self, ctxt)
  ctxt:add_effect(    E.Read( self.base.type, path, self.srcinfo ))
  self.base:effectcheck(ctxt)
end
function AST.TableWrite:effectcheck(ctxt)
  INTERNAL_ERR('Should Never Call Directly')
end
function AST.GlobalRead:effectcheck(ctxt)
  local path        = get_readwrite_path(self, ctxt)
  ctxt:add_effect(    E.ReadG( self.base, path, self.srcinfo ))
end
function AST.GlobalWrite:effectcheck(ctxt)
  INTERNAL_ERR('Should Never Call Directly')
end


--------------------------------------

function AST.BinaryOp:effectcheck(ctxt)
  self.lhs:effectcheck(ctxt)
  self.rhs:effectcheck(ctxt)
end
function AST.UnaryOp:effectcheck(ctxt)
  self.expr:effectcheck(ctxt)
end
function AST.TernaryExpr:effectcheck(ctxt)
  self.cond:effectcheck(ctxt)
  self.lhs:effectcheck(ctxt)
  self.rhs:effectcheck(ctxt)
end
function AST.LetExpr:effectcheck(ctxt)
  ctxt:enter_letblock()
  self.block:effectcheck(ctxt)
  ctxt:leave_letblock()
  self.expr:effectcheck(ctxt)
end

--------------------------------------

function AST.Cast:effectcheck(ctxt)
  self.arg:effectcheck(ctxt)
end
function AST.BuiltInStmt:effectcheck(ctxt)
  effectcheck_all(self.args, ctxt)
  local effects     = self.builtin._effectcheck(self.args, self, ctxt)
  for _,eff in ipairs(effects) do
    ctxt:add_effect(eff)
  end
end
function AST.BuiltInCall:effectcheck(ctxt)
  effectcheck_all(self.args, ctxt)
  local effects     = self.builtin._effectcheck(self.args, self, ctxt)
  for _,eff in ipairs(effects) do
    ctxt:add_effect(eff)
  end
end
function AST.FuncCall:effectcheck(ctxt)
  effectcheck_all(self.args, ctxt)
  local effects     = self.func:_INTERNAL_geteffects()
  for _,e in ipairs(effects) do
    ctxt:add_effect(e)
  end
  local subfuncs    = self.func:_INTERNAL_getsubfuncs()
  ctxt:add_funcs(subfuncs)
  ctxt:add_func(self.func)
end
function AST.TensorMap:effectcheck(ctxt)
  for _,nm in ipairs(self.names) do ctxt:definevar(nm) end
  self.expr:effectcheck(ctxt)
end
function AST.TensorFold:effectcheck(ctxt)
  for _,nm in ipairs(self.names) do ctxt:definevar(nm) end
  self.expr:effectcheck(ctxt)
end




