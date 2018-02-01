local Exports = {}
package.loaded["gong.src.codegen"] = Exports

-------------------------------------------------------------------------------

local Typechecker = require 'gong.src.typechecker'
local T           = require 'gong.src.types'
local Util        = require 'gong.src.util'
local AST         = Typechecker.AST
--local is_type     = T.is_type
--local SrcInfo     = Util.SrcInfo
--local NewSymbol   = Util.NewSymbol
--local is_symbol   = Util.is_symbol
--local is_id_str   = Util.is_id_str
--local is_int      = Util.is_int
local INTERNAL_ERR  = Util.INTERNAL_ERR

local Schemata    = require 'gong.src.schemata'
--local Global      = require 'gong.src.global'
local Functions   = require 'gong.src.functions'
local is_function = Functions.is_function
local is_builtin  = Functions.is_builtin

local newlist   = terralib.newlist


-------------------------------------------------------------------------------
--[[                          Context Definition                           ]]--
-------------------------------------------------------------------------------
local Context = {}
Context.__index = Context

local function NewContext(StoreWrap)
  local ctxt = setmetatable({
    env           = terralib.newenvironment(nil),
    _W            = StoreWrap,
  }, Context)
  return ctxt
end
function Context:localenv()
  return self.env:localenv()
end

function Context:GetTerraFunction(obj)
  if is_function(obj) then
    return self._W:get_terra_function(obj)
  else INTERNAL_ERR('cannot get Terra function for object type') end
end

function Context:StorePtr()
  if self._store_ptr then return self._store_ptr end
  local ptr               = symbol( &(self._W:Store_Struct()),
                                    'store' )
  self._store_ptr         = ptr
  return ptr
end
function Context:NewSym(name, type)
  local ttype             = type:terratype()
  local displayname       = (name and tostring(name)) or nil
  local sym               = symbol( ttype, displayname )
  if name then
    self:localenv()[name] = sym
  end
  return sym
end
function Context:GetSym(name)
  return self:localenv()[name]
end

function Context:Scan(srctype, row, code)
  return self._W:Scan(self:StorePtr(), srctype, row, code)
end

function Context:Read(srctype, row, path)
  return self._W:Read(self:StorePtr(), srctype, row, path)
end

function Context:Write(dsttype, row, path, rval)
  return self._W:Write(self:StorePtr(), dsttype, row, path, rval)
end

function Context:Reduce(dsttype, op, row, path, rval)
  return self._W:Reduce(self:StorePtr(), dsttype, op, row, path, rval)
end

function Context:Insert(dsttype, vals)
  return self._W:Insert(self:StorePtr(), dsttype, vals)
end

-------------------------------------------------------------------------------
--[[                               Operators                               ]]--
-------------------------------------------------------------------------------

local minf = macro(function(a,b)
  return quote
    var r = [a]
    var t = [b]
  in terralib.select(t < r, t, r) end
end)
local maxf = macro(function(a,b)
  return quote
    var r = [a]
    var t = [b]
  in terralib.select(t > r, t, r) end
end)

-- The following are all scalar operator generation for single-core CPU
local hugeDouble      = math.huge

local INIT_REDOP = macro(function(op, typ, arg)
  assert(typ:is_primitive(), 'INTERNAL: expect primitive type')
  op = op:asvalue()
  assert(type(op) == 'string', 'INTERNAL: expect string operator')

  -- + and * will be handled by the type of arg
  if      op == '+'   then  return quote [arg] = 0 end
  elseif  op == '*'   then  return quote [arg] = 1 end
  -- min and max initialization change value depending on the type...
  elseif  op == 'min' or op == 'max' then
    local ttype = typ:terratype()
    if typ == T.double or typ == T.float then
      local inf = math.huge
      if op == 'max' then inf = -inf end
      return quote [arg] = [ttype](inf) end
    elseif not typ:is_signed() then
      if op == 'max' then return quote [arg] = 0 end
      else -- op == 'min'
        if      ttype == uint8  then return quote [arg] = 0xFF       end
        elseif  ttype == uint16 then return quote [arg] = 0xFFFF     end
        elseif  ttype == uint32 then return quote [arg] = 0xFFFFFFFF end
        elseif  ttype == uint64 then
                          return quote [arg] = 0xFFFFFFFFFFFFFFFFULL end
        else INTERNAL_ERR() end
      end
    else
      if op == 'min' then
        if      ttype == int8  then return quote [arg] = 0x80       end
        elseif  ttype == int16 then return quote [arg] = 0x8000     end
        elseif  ttype == int32 then return quote [arg] = 0x80000000 end
        elseif  ttype == int64 then
                          return quote [arg] = 0x8000000000000000LL end
        else INTERNAL_ERR() end
      else -- op == 'max'
        if      ttype == int8  then return quote [arg] = -0x7F       end
        elseif  ttype == int16 then return quote [arg] = -0x7FFF     end
        elseif  ttype == int32 then return quote [arg] = -0x7FFFFFFF end
        elseif  ttype == int64 then
                          return quote [arg] = -0x7FFFFFFFFFFFFFFFLL end
        else INTERNAL_ERR() end
      end
    end
    return quote [arg] = [math.huge]  end

  else INTERNAL_ERR('unexpected reduction op: '..tostring(op)) end
end)

local REDUCE_OP = macro(function(op, lhs, rhs)
  op = op:asvalue()
  assert(type(op) == 'string', 'INTERNAL: expect string operator')
  if      op == '+'   then  return quote [lhs] = [lhs] + [rhs] end
  elseif  op == '*'   then  return quote [lhs] = [lhs] * [rhs] end
  elseif  op == 'min' then  return quote [lhs] = minf([lhs], [rhs]) end
  elseif  op == 'max' then  return quote [lhs] = maxf([lhs], [rhs]) end
  else INTERNAL_ERR('unexpected reduction op: '..tostring(op)) end
end)

local BINARY_OP = macro(function(op, lhs, rhs)
  op = op:asvalue()
  assert(type(op) == 'string', 'INTERNAL: expect string operator')
  if      op == '+'   then  return `[lhs] + [rhs]
  elseif  op == '-'   then  return `[lhs] - [rhs]
  elseif  op == '*'   then  return `[lhs] * [rhs]
  elseif  op == '/'   then  return `[lhs] / [rhs]
  elseif  op == '%'   then  return `[lhs] % [rhs]
  elseif  op == '<'   then  return `[lhs] < [rhs]
  elseif  op == '>'   then  return `[lhs] > [rhs]
  elseif  op == '<='  then  return `[lhs] <= [rhs]
  elseif  op == '>='  then  return `[lhs] >= [rhs]
  elseif  op == '=='  then  return `[lhs] == [rhs]
  elseif  op == '~='  then  return `[lhs] ~= [rhs]
  elseif  op == 'and' then  return `[lhs] and [rhs]
  elseif  op == 'or'  then  return `[lhs] or [rhs]
  else INTERNAL_ERR('unexpected binary op: '..tostring(op)) end
end)

local UNARY_OP = macro(function(op, arg)
  op = op:asvalue()
  assert(type(op) == 'string', 'INTERNAL: expect string operator')
  local ttyp = typ:terratype()
  if      op == '-'   then  return `-[arg]
  elseif  op == 'not' then  return `not [arg]
  else INTERNAL_ERR('unexpected unary op: '..tostring(op)) end
end)

Exports.INIT_REDOP    = INIT_REDOP
Exports.REDUCE_OP     = REDUCE_OP
Exports.BINARY_OP     = BINARY_OP
Exports.UNARY_OP      = UNARY_OP


-------------------------------------------------------------------------------
--[[                              Entry Point                              ]]--
-------------------------------------------------------------------------------

function Exports.codegen(name, ast, StoreWrap)
  local ctxt        = NewContext(StoreWrap)
  local func        = ast:codegen(ctxt)
  func:setname(name)
  return func
end

local function codegen_all(xs, ctxt)
  return xs:map(function(x) return x:codegen(ctxt) end)
end


-------------------------------------------------------------------------------
--[[                            Top-Level Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Function:codegen(ctxt)
  local fargs           = codegen_all(self.args, ctxt)
  local body            = self.body:codegen(ctxt)
  local rettyp          = self.rettype:terratype()
  local args            = newlist{ ctxt:StorePtr() }
  args:insertall(fargs)

  local func = terra( [args] ) : rettyp [body] end
  return func
end

function AST.Join:codegen(ctxt)
  assert(#self.args >= 2 and self.args[1].type:is_row()
                         and self.args[2].type:is_row(), 'INTERNAL')
  local fargs           = codegen_all(self.args, ctxt)
  local typA, typB      = self.args[1].type, self.args[2].type
  local rowA, rowB      = fargs[1], fargs[2]
  local innerargs       = newlist{ ctxt:StorePtr() }
  innerargs:insertall(fargs)
  for k=1,#self.args do fargs[k] = fargs[k+2] end
  local outerargs       = newlist{ ctxt:StorePtr() }
  outerargs:insertall(fargs)

  local filter          = self.filter:codegen(ctxt)
  local doblock         = self.doblock:codegen(ctxt)

  local innerloop = terra( [innerargs] ) : bool
    [filter]
    [doblock]
    return true
  end
  local outerloop = terra( [outerargs] )
    -- blah
    [ ctxt:Scan(typA, rowA,
        ctxt:Scan(typB, rowB, quote
          innerloop( [innerargs] )
        end))]
  end
  return outerloop, newlist{ innerloop }
end

function AST.ArgDecl:codegen(ctxt)
  return ctxt:NewSym(self.name, self.type)
end


-------------------------------------------------------------------------------
--[[                            Statement Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Block:codegen(ctxt)
  local stmts     = newlist()
  for _,s in ipairs(self.stmts) do
    stmts:insert(   s:codegen(ctxt) )
  end
  return stmts
end

function AST.WhereFilter:codegen(ctxt)
  local expr      = self.expr:codegen(ctxt)
  return quote
    if not [expr] then return false end
  end
end
function AST.EmitStmt:codegen(ctxt)
  local exprs     = codegen_all(self.record.exprs, ctxt)
  local dst       = self.dst
  return ctxt:Insert(dst, exprs)
end
function AST.ReturnStmt:codegen(ctxt)
  local expr      = self.expr:codegen(ctxt)
  return quote
    return [expr]
  end
end


function AST.PathField:codegen(ctxt)
  return self.name
end
function AST.PathIndex:codegen(ctxt)
  return codegen_all(self.args, ctxt)
end
function AST.Assignment:codegen(ctxt)
  local rval      = self.rval:codegen(ctxt)
  if AST.TableWrite.check(self.lval) then
    local row     = self.lval.base:codegen(ctxt)
    local path    = codegen_all(self.lval.path, ctxt)
    local dst     = self.lval.base.type
    return ctxt:Write(dst, row, path, rval)
  else
    local lval    = self.lval:codegen(ctxt)
    return quote [lval] = [rval] end
  end
end
function AST.DeclStmt:codegen(ctxt)
  local symname   = ctxt:NewSym(self.name, self.type)
  local rval      = self.rval:codegen(ctxt)
  return quote var [symname] = [rval] end
end
local function reduction_lval_unwind(lval, ctxt)
  if AST.RecordRead.check(lval) then
    local expr, stmts     = reduction_lval_unwind(lval.base, ctxt)
    return (`expr.[lval.arg]), stmts
  elseif AST.TensorIndex.check(lval) then
    local expr, stmts     = reduction_lval_unwind(lval.base, ctxt)
    -- cache computed offset
    local offset          = `0
    local strides         = lval.base.type.rowstrides
    for i,a in ipairs(lval.args) do
      local aexpr         = a:codegen(ctxt)
      local off           = offset
      offset              = `[off] + [ strides[i] ] * [aexpr]
    end
    local offname         = ctxt:NewSym(nil, lval.args[1].type)
    stmts:insert(quote var [offname] = [offset] end)
    return (`expr.d[ offname ]), stmts
  elseif AST.Var.check(lval) then
    return lval:codegen(ctxt), newlist()
  else INTERNAL_ERR("Unexpected lval") end
end
function AST.Reduction:codegen(ctxt)
  local rval      = self.rval:codegen(ctxt)
  if AST.TableWrite.check(self.lval) then
    local row     = self.lval.base:codegen(ctxt)
    local path    = codegen_all(self.lval.path, ctxt)
    local dst     = self.lval.base.type
    return ctxt:Reduce(dst, self.op, row, path, rval)
  else
    local lval, stmts   = reduction_lval_unwind(self.lval, ctxt)
    local op            = self.op
    local typ           = self.lval.type
    if typ:is_tensor() then
      local btyp        = typ:basetype()
      local Nd          = typ._n_entries
      return quote
        [stmts]
        var rhs = [rval]
        for k=0,Nd do REDUCE_OP(op, lval.d[k], rhs.d[k]) end
      end
    else
      return quote
        [stmts]
        REDUCE_OP(op, lval, rval)
      end
    end
  end
end

function AST.IfStmt:codegen(ctxt)
  local stmt          = newlist()
  if self.else_body then
    stmt              = self.else_body:codegen(ctxt)
  end
  for i=#self.cases,1,-1 do
    local cond        = self.cases[i].cond:codegen(ctxt)
    local body        = self.cases[i].body:codegen(ctxt)
    local prev        = stmt
    stmt              = quote if [cond] then [body] else [prev] end end
  end
  return stmt
end
function AST.ForLoop:codegen(ctxt)
  local itername      = ctxt:NewSym(self.itername, self.lo.type)
  local lo            = self.lo:codegen(ctxt)
  local hi            = self.hi:codegen(ctxt)
  local stride        = (self.stride and self.stride:codegen(ctxt)) or nil
  local body          = self.body:codegen(ctxt)
  if stride then
    return quote for [itername] = [lo],[hi],[stride] do [body] end end
  else
    return quote for [itername] = [lo],[hi] do [body] end end
  end
end


-------------------------------------------------------------------------------
--[[                            Expression Rules                           ]]--
-------------------------------------------------------------------------------

function AST.Var:codegen(ctxt)
  local symname       = ctxt:GetSym(self.name)
  return symname
end
function AST.LuaObj:codegen(ctxt)
  INTERNAL_ERR("Should not have LuaObjects left at Codegen")
end
function AST.NumLiteral:codegen(ctxt)
  local ttype         = self.type:terratype()
  return `[ttype]( [self.value] )
end
function AST.BoolLiteral:codegen(ctxt)
  return self.value
end

--------------------------------------

function AST.RecordExpr:codegen(ctxt)
  local exprs         = codegen_all(self.exprs, ctxt)
  local ttype         = self.type:terratype()
  return `[ttype]({ [exprs] })
end
function AST.ListExpr:codegen(ctxt)
  local exprs         = codegen_all(self.exprs, ctxt)
  local ttype         = self.type:terratype()
  local stride        = self.type.rowstrides[#self.type.rowstrides]

  if stride == 1 then
    return `[ttype]({ d=array( [exprs] ) })
  else
    return quote
      var res : ttype
      escape for k,e in ipairs(exprs) do
        local base    = stride * (k-1)
        emit quote
          var tempval = [e] -- bind to prevent recompute
          for i=0,stride do res.d[base+i] = tempval.d[i] end -- copy
        end
      end end
    in res end
  end
end

--------------------------------------

function AST.TensorIndex:codegen(ctxt)
  local strides       = self.base.type.rowstrides
  local base          = self.base:codegen(ctxt)
  local args          = codegen_all(self.args, ctxt)
  local offset        = args[1]
  for k=2,#args do
    local off         = offset
    offset            = `[off] + [ strides[k] ] * [ args[k] ]
  end
  return `base.d[ offset ]
end
function AST.RecordRead:codegen(ctxt)
  local base          = self.base:codegen(ctxt)
  return `base.[ self.arg ]
end
function AST.TableRead:codegen(ctxt)
  local row           = self.base:codegen(ctxt)
  local path          = codegen_all(self.path, ctxt)
  local dst           = self.base.type
  return ctxt:Read(dst, row, path)
end
function AST.TableWrite:codegen(ctxt)
  INTERNAL_ERR('Should Never Call Directly')
end

--------------------------------------

function AST.BinaryOp:codegen(ctxt)
  local ltyp          = self.lhs.type
  local rtyp          = self.rhs.type
  local typ           = self.type
  local op            = self.op
  local lhs           = self.lhs:codegen(ctxt)
  local rhs           = self.rhs:codegen(ctxt)
  if ltyp:is_primitive() and rtyp:is_primitive() then
    return `BINARY_OP(op,lhs,rhs)
  elseif op == '==' or op == '~=' then
    local btyp        = ltyp:basetype()
    local Nd          = typ._n_entries
    local foldop      = macro(function(a,b)
                                if op == '==' then return `a and b
                                              else return `a or b end end)
    return quote
      var res : bool  = [ (op == '==' and true ) or false ]
      var lv          = [lhs]
      var rv          = [rhs]
      for i=0,Nd do
        res = foldop( res, BINARY_OP(op, lv.d[i], rv.d[i]) )
      end
    in res end
  else
    local ttype       = typ:terratype()
    local btyp        = typ:basetype()
    local Nd          = typ._n_entries
    return quote
      var res : ttype
      var lv          = [lhs]
      var rv          = [rhs]
      for i=0,Nd do escape
        local lval    = (ltyp:is_tensor() and `lv.d[i]) or `lv
        local rval    = (rtyp:is_tensor() and `rv.d[i]) or `rv
        emit quote res.d[i] = BINARY_OP(op, lval, rval) end
      end end
    in res end
  end
end
function AST.UnaryOp:codegen(ctxt)
  local typ           = self.type
  local op            = self.op
  local expr          = self.expr:codegen(ctxt)
  if typ:is_primitive() then
    return `UNARY_OP(op,expr)
  else
    local ttype       = typ:terratype()
    local btyp        = typ:basetype()
    local Nd          = typ._n_entries
    return quote
      var res : ttype
      var e = [expr]
      for i=0,Nd do res.d[i] = UNARY_OP(op, e.d[i]) end
    end
  end
end

function AST.TernaryExpr:codegen(ctxt)
  local cond          = self.cond:codegen(ctxt)
  local lhs           = self.lhs:codegen(ctxt)
  local rhs           = self.rhs:codegen(ctxt)
  return `terralib.select(cond, lhs, rhs)
end
function AST.LetExpr:codegen(ctxt)
  local stmts         = self.block:codegen(ctxt)
  local expr          = self.expr:codegen(ctxt)
  return quote [stmts] in [expr] end
end

--------------------------------------

function AST.Cast:codegen(ctxt)
  local ttype         = self.type:terratype()
  local arg           = self.arg:codegen(ctxt)
  if self.type:is_tensor() then
    local Nelem       = self.type._n_entries
    local btype       = self.type:terrabasetype()
    return quote
      var res : ttype
      var a   = [arg] -- bind to prevent re-evaluation
      for k=0,Nelem do res.d[k] = [btype]( a.d[k] ) end
    in res end
  else
    return `[ttype](arg)
  end
end
function AST.BuiltInCall:codegen(ctxt)
  local args          = codegen_all(self.args, ctxt)
  local call          = self.builtin._codegen(ctxt, args, self.args)
  return call
end
function AST.FuncCall:codegen(ctxt)
  local args          = codegen_all(self.args, ctxt)
  local func          = ctxt:GetTerraFunction(self.func)
  local storeptr      = ctxt:StorePtr()
  return `func( storeptr, [args] )
end

-- WARNING: Destructive to names and dims lists
local function tensorop_loops( names, dims, body )
  if #names == 0 then
    return body
  else
    local nm            = names:remove()
    local d             = dims:remove()
    local subcall       = tensorop_loops(names, dims, body)
    return quote for [nm]=0,d do [subcall] end end
  end
end
local function tensorop_prelude(self, ctxt)
  local names         = newlist()
  local mapdims       = newlist()
  for i,nm in ipairs(self.names) do
    names[i]          = ctxt:NewSym(nm, self.idxtypes[i])
    mapdims[i]        = self.idxtypes[i].range
  end
  local expr          = self.expr:codegen(ctxt)
  return names, mapdims, expr
end
function AST.TensorMap:codegen(ctxt)
  local names, mapdims, expr = tensorop_prelude(self, ctxt)

  local ttype         = self.type:terratype()
  local strides       = self.type.rowstrides
  local etype         = self.expr.type
  local ed            = (etype:is_tensor() and etype._n_entries) or 1

  local offset        = `[ ed*strides[1] ] * [ names[1] ]
  for k=2,#names do
    local off         = offset
    offset            = `[off] + [ ed*strides[k] ] * [ names[k] ]
  end

  return quote
    var res : ttype
    escape if etype:is_tensor() then
      emit(tensorop_loops(names, mapdims, quote
        var etmp          = [expr]
        for k=0,ed do
          res.d[offset+k] = etmp.d[k]
        end
      end))
    else
      emit(tensorop_loops(names, mapdims, quote
        res.d[offset]     = [expr]
      end))
    end end
  in res end
end
function AST.TensorFold:codegen(ctxt)
  local names, mapdims, expr = tensorop_prelude(self, ctxt)

  local op            = self.op
  local typ           = self.type
  local btyp          = (typ:is_tensor() and typ:basetype()) or nil
  local ttype         = self.type:terratype()
  local td            = (typ:is_tensor() and typ._n_entries) or 1

  return quote
    var res : ttype
    escape if etype:is_tensor() then
      emit quote
      for k=0,td do INIT_REDOP(op, btyp, res.d[k]) end
      [tensorop_loops(names, mapdims, quote
        var etmp          = [expr]
        for k=0,td do REDUCE_OP(op, res.d[k], etmp.d[k]) end
      end)] end
    else
      emit quote
      INIT_REDOP(op, typ, res)
      [tensorop_loops(names, mapdims, quote
        REDUCE_OP(op, res, expr)
      end)] end
    end end
  in res end
end




