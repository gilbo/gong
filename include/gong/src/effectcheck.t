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

local newlist   = terralib.newlist


-------------------------------------------------------------------------------
--[[                                Effects                                ]]--
-------------------------------------------------------------------------------

local redop_token_set       = Typechecker.redop_token_set
Exports.redop_token_set     = redop_token_set

local ADT E
  
  Effect = Scan     { src  : Type }
         | Filter   {}
         | Emit     { dst  : Type } -- destination table
         | Return   { type : Type }
         | Write    { dst  : Type,        path : PathToken* }
         | Reduce   { op   : redop,
                      dst  : Type,        path : PathToken* }
         | Read     { src  : Type,        path : PathToken* }
        attributes {                                    srcinfo : SrcInfo }

  PathToken = FieldToken { name : id_str }
            | IndexToken { } -- assume all of the indices are hit for now

  extern redop    function(obj) return redop_token_set[obj] end

  extern Type     is_type

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
    env           = terralib.newenvironment(nil),
    diag          = terralib.newdiagnostics(),
    _let_depth    = 0,
    _mode         = nil,
    _permissions  = {},
    _scan_tables  = {},
    _all_effects  = newlist(),
    _all_subfuncs = {},
  }, Context)
  return ctxt
end
function Context:localenv()
  return self.env:localenv()
end
function Context:enterblock()
  self.env:enterblock()
end
function Context:leaveblock()
  self.env:leaveblock()
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

function Context:enter_letblock()   self._let_depth = self._let_depth+1   end
function Context:leave_letblock()   self._let_depth = self._let_depth-1   end
function Context:enter_function()   self._mode      = 'function'          end
function Context:leave_function()   self._mode      = nil                 end
function Context:enter_joinfilter() self._mode      = 'filter'            end
function Context:leave_joinfilter() self._mode      = nil                 end
function Context:enter_joindo()     self._mode      = 'do'                end
function Context:leave_joindo()     self._mode      = nil                 end

function Context:in_letblock()      return self._let_depth > 0      end
function Context:in_function()      return self._mode == 'function' end
function Context:in_joinfilter()    return self._mode == 'filter'   end
function Context:in_joindo()        return self._mode == 'do'       end

function Context:add_effect(eff)
  local err         = false
  if E.Read.check(eff) then -- pretty generic
    if not self:try_read(eff, eff.src:table(), eff.path) then
      err = true end

  elseif self:in_letblock() then
    err = true
    self:error(eff, "no effects are allowed inside of let-expression "..
                    "statement blocks")

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
    else
      if not self:try_write(eff, eff.dst:table(), newlist()) then
        err = true end
    end

  elseif E.Reduce.check(eff) then
    if not self:in_joindo() then
      err = true
      self:error(eff,
        "reductions are only allowed inside join do blocks")
    else
      if not self:try_reduce(eff, eff.dst:table(), eff.path) then
        err = true end
    end

  elseif E.Write.check(eff) then
    err = true
    self:error(eff,
      "writing to table fields is not allowed in gong")

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

  for _,f in ipairs(tbl:fields()) do
    p[f:name()]       = {}
    expand_field(p[f:name()], f:type())
  end

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
    if f.mode and f.mode ~= new_mode then
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


-------------------------------------------------------------------------------
--[[                              Entry Point                              ]]--
-------------------------------------------------------------------------------

function Exports.effectcheck(input_ast)
  local ctxt        = NewContext()

  ctxt:enterblock()
  local effects     = input_ast:effectcheck(ctxt)
  ctxt:leaveblock()
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
  -- nada
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
  self.record:effectcheck(ctxt)
  ctxt:add_effect(  E.Emit(self.dst, self.srcinfo) )
end
function AST.ReturnStmt:effectcheck(ctxt)
  self.expr:effectcheck(ctxt)
  ctxt.return_flag  = true
  ctxt:add_effect(  E.Return(self.expr.type, self.srcinfo) )
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
    ctxt:add_effect(  E.Write( lval.type, path, self.srcinfo ) )
  end
  lval:effectcheck(ctxt)
  self.rval:effectcheck(ctxt)
end
function AST.DeclStmt:effectcheck(ctxt)
  self.rval:effectcheck(ctxt)
end
function AST.Reduction:effectcheck(ctxt)
  local lval        = self.lval
  -- check for write?
  if AST.TableWrite.check(lval) then
    local path      = get_readwrite_path(lval, ctxt)
    lval            = lval.base
    ctxt:add_effect(  E.Reduce( self.op, lval.type, path, self.srcinfo ) )
  end
  lval:effectcheck(ctxt)
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
  self.lo:effectcheck(ctxt)
  self.hi:effectcheck(ctxt)
  if self.stride then
    self.stride:effectcheck(ctxt)
  end
  self.body:effectcheck(ctxt)
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
function AST.BuiltInCall:effectcheck(ctxt)
  effectcheck_all(self.args, ctxt)
  local effects     = self.builtin._effectcheck(unpack(self.args))
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
  self.expr:effectcheck(ctxt)
end
function AST.TensorFold:effectcheck(ctxt)
  self.expr:effectcheck(ctxt)
end




