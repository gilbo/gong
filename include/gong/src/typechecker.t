import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.typechecker"] = Exports

-------------------------------------------------------------------------------

local Specializer = require 'gong.src.specializer'
local T           = require 'gong.src.types'
local Util        = require 'gong.src.util'
local AST         = Specializer.AST
local is_type     = T.is_type
local SrcInfo     = Util.SrcInfo
local NewSymbol   = Util.NewSymbol
local is_symbol   = Util.is_symbol
local is_id_str   = Util.is_id_str
local is_int      = Util.is_int
local INTERNAL_ERR  = Util.INTERNAL_ERR

local Schemata    = require 'gong.src.schemata'
local Macro       = require 'gong.src.macro'
local Global      = require 'gong.src.global'
local Functions   = require 'gong.src.functions'
local is_macro    = Macro.is_macro
local is_quote    = Macro.is_quote
local is_constant = Global.is_constant
local is_function = Functions.is_function
local is_builtin  = Functions.is_builtin

local newlist   = terralib.newlist

-------------------------------------------------------------------------------
--                                    AST                                    --
-------------------------------------------------------------------------------

local binop_token_set       = Specializer.binop_token_set
local unop_token_set        = Specializer.unop_token_set
local redop_token_set       = Specializer.redop_token_set

Exports.binop_token_set     = binop_token_set
Exports.unop_token_set      = unop_token_set
Exports.redop_token_set     = redop_token_set

local function is_gong_obj(obj)
  return is_macro(obj) or is_quote(obj) or type(obj) == 'string'
                       or is_function(obj) or is_builtin(obj)
                       or is_type(obj)
end
local gong_obj_err_msg = "expected a string, Gong Macro, Quote, or Function"

local ADT A
  -- Top Level Forms
  Function    = { args    : ArgDecl*,
                  rettype : Type,
                  body    : Block,            srcinfo : SrcInfo }
  Join        = { args    : ArgDecl*,
                  filter  : Block,            
                  doblock : Block,            srcinfo : SrcInfo }

  -- Shared Structural Components
  ArgDecl     = { name    : Symbol,
                  type    : Type,             srcinfo : SrcInfo }
  IfCase      = { cond    : Expr,
                  body    : Block,            srcinfo : SrcInfo }
  Block       = { stmts   : Stmt*,            srcinfo : SrcInfo }

  -- Statements
  Stmt  =
    -- Terminal / Effect Statements
          WhereFilter { expr      : Expr }
        | EmitStmt    { record    : RecordExpr,
                        dst       : Type }
        | ReturnStmt  { expr      : Expr }
    -- Binding & Assignment
        | Assignment  { lval      : Expr,     rval      : Expr  }
        | DeclStmt    { name      : Symbol,   type      : Type,
                        rval      : Expr                        }
        | Reduction   { op        : redop,
                        lval      : Expr,     rval      : Expr  }
    -- Looping & Branching
        | IfStmt      { cases     : IfCase*,  else_body : Block? }
        | ForLoop     { itername  : Symbol,
                        lo        : Expr,     hi        : Expr,
                        stride    : Expr?,
                        body      : Block }
        attributes {                          srcinfo : SrcInfo }

  -- Expressions
  Expr =
    -- Literals and other Atoms
          Var         { name  : Symbol }
        | LuaObj      { }               -- intermediate use only
        | NumLiteral  { value : number }
        | BoolLiteral { value : boolean }
    -- Data Constructors
        | RecordExpr  { exprs : Expr* }
        | ListExpr    { exprs : Expr* }
    -- Data Destructors
        -- lookup is base[a1,...]
        | TensorIndex { base  : Expr,         args      : Expr* }
        | RecordRead  { base  : Expr,         arg     : id_str  }
        | TableRead   { base  : Expr,         path : PathToken* }
        | TableWrite  { base  : Expr,         path : PathToken* }
    -- Basic building up of conditions and terms
        | BinaryOp    { op    : binop,
                        lhs   : Expr,         rhs       : Expr  }
        | UnaryOp     { op    : unop,         expr      : Expr  }
        | TernaryExpr { cond  : Expr,
                        lhs   : Expr,         rhs       : Expr  }
        | LetExpr     { block : Block,        expr      : Expr  }
    -- Special forms
        | Cast        { arg   : Expr                            }
        | BuiltInCall { builtin : BuiltIn,    args      : Expr* }
        | FuncCall    { func  : FuncObj,      args      : Expr* }
        | TensorMap   { names : Symbol*,      idxtypes  : Type*,
                                              expr      : Expr  }
        | TensorFold  { op    : redop,
                        names : Symbol*,      idxtypes  : Type*,
                                              expr      : Expr  }
        attributes {    type  : Type,         srcinfo : SrcInfo }

  PathToken = PathField { name : id_str }
            | PathIndex { args : Expr*  }
            attributes {                      srcinfo : SrcInfo }

  extern binop  function(obj) return binop_token_set[obj] end
  extern unop   function(obj) return unop_token_set[obj]  end
  extern redop  function(obj) return redop_token_set[obj] end

  extern Type     is_type
  extern Symbol   is_symbol
  extern BuiltIn  is_builtin
  extern FuncObj  is_function

  extern id_str   is_id_str
  extern SrcInfo  function(obj) return SrcInfo.check(obj) end
end
Exports.AST = A


-------------------------------------------------------------------------------
--[[                          Context Definition                           ]]--
-------------------------------------------------------------------------------
local Context = {}
Context.__index = Context

local function NewContext()
  local ctxt = setmetatable({
    env       = terralib.newenvironment(nil),
    diag      = terralib.newdiagnostics(),
    _rettype  = T.error,
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

-- merge handling of tensorindices and other types
function Context:settype(sym, typ)
  assert(is_type(typ), "INTERNAL: tried to set non-type")
  self:localenv()[sym] = typ
end
function Context:set_tensorindex(sym, typ, ast)
  local lookup = self:localenv()[sym]
  if lookup then
    if lookup.type == T.tensorindex() then
      if typ == T.tensorindex() then
        lookup.asts:insert(ast)
      else
        -- patch up previous sites now that we've
        -- inferred the index type
        for _,a in ipairs(lookup.asts) do
          a.type = typ
        end
      end
    end
    lookup.type = typ
  else
    lookup = {  type = typ,
                asts = newlist { ast }  }
    self:localenv()[sym] = lookup
  end
end
function Context:gettype(sym)
  local typ = self:localenv()[sym]
  if typ == nil or is_type(typ) then  return typ
                                else  return typ.type   end
end

function Context:get_return()
  return self._rettype
end
function Context:add_return(typ, ast)
  if self._rettype ~= typ and self._rettype ~= T.error and typ ~= T.error then
    self:error(ast, "inconsistent return type: "..typ)
  else
    self._rettype = typ
  end
end


-------------------------------------------------------------------------------
--[[                              Entry Point                              ]]--
-------------------------------------------------------------------------------

function Exports.typecheck(input_ast)
  local ctxt    = NewContext()

  ctxt:enterblock()
  local output_obj = input_ast:typecheck(ctxt)
  ctxt:leaveblock()
  ctxt:finishandabortiferrors(3)

  return output_obj
end

local function do_coercion(ast, targettype, ctxt)
  if ast.type == targettype then -- shortcut
    return ast
  else
    -- return a cast regardless then, but
    -- if the cast is not good, report an error and set the error
    -- type on the cast!
    if not ast.type:is_coercable_to(targettype) then
      ctxt:error(ast, "Could not coerce expression of type '"..
                      tostring(ast.type) .. "' into type '"..
                      tostring(targettype) .. "'")
      targettype = T.error
    end
    return A.Cast(ast, targettype, ast.srcinfo)
  end
end

local function typecheck_all(xs, ctxt)
  return xs:map(function(x) return x:typecheck(ctxt) end)
end


-------------------------------------------------------------------------------
--[[                            Top-Level Rules                            ]]--
-------------------------------------------------------------------------------

function AST.QuoteExpr:typecheck(ctxt)
  local expr      = self.expr:typecheck(ctxt)
  local q         = Macro.NewQuote(expr, self.srcinfo, false)
  return q
end
function AST.QuoteStmt:typecheck(ctxt)
  local block     = self.block:typecheck(ctxt)
  local q         = Macro.NewQuote(block, self.srcinfo, true)
  return q
end

function AST.Function:typecheck(ctxt)
  local args, argtypes = newlist(), newlist()
  for _,a in ipairs(self.args) do
    a = a:typecheck(ctxt)
    args:insert( a )
    argtypes:insert( a.type )
  end

  if self.rettype ~= nil then ctxt:add_return(self.rettype, self) end
  local body      = self.body:typecheck(ctxt)
  local rettype   = ctxt:get_return()
  -- TODO: This typechecker does not guarantee that every
  --       function will return some value.
  --       More involved analysis is necessary to guarantee that
  --       in the presence of if-then-else conditional branching
  --  Idea: maybe address this is a second-stage "effect-check"
  if rettype == T.error then
    ctxt:error(self, 'could not infer function return type')
  elseif not rettype:is_value() then
    ctxt:error(self, 'function must return a value type, not: '..
                     tostring(rettype))
  end
  local ast       = A.Function(args, rettype, body, self.srcinfo)

  return Functions.NewFunction{
    name      = self.name,
    argtypes  = argtypes,
    rettype   = rettype,
    ast       = ast,
  }
end

function AST.Join:typecheck(ctxt)
  local args, argtypes = newlist(), newlist()
  for _,a in ipairs(self.args) do
    a = a:typecheck(ctxt)
    args:insert( a )
    argtypes:insert( a.type )
  end
  -- the first two arguments must be tables
  if #args < 2 then
    ctxt:error(self, "expected at least 2 arguments to join") end
  if #args >= 1 and not argtypes[1]:is_row() then
    ctxt:error(self, "expected first argument to be a table row") end
  if #args >= 2 and not argtypes[2]:is_row() then
    ctxt:error(self, "expected second argument to be a table row") end

  local filter    = self.filter:typecheck(ctxt)
  local doblock   = self.doblock:typecheck(ctxt)
  local ast       = A.Join(args, filter, doblock, self.srcinfo)

  return Functions.NewJoin{
    name        = self.name,
    argtypes    = argtypes,
    ast         = ast,
  }
end

function AST.ArgDecl:typecheck(ctxt)
  if not self.type:is_value() then
    ctxt:error(self, "expected argument to have a value type")
  end
  ctxt:settype(self.name, self.type)
  return A.ArgDecl(self.name, self.type, self.srcinfo)
end


-------------------------------------------------------------------------------
--[[                            Statement Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Block:typecheck(ctxt)
  local stmts     = newlist()
  for _,s in ipairs(self.stmts) do
    local ss      = s:typecheck(ctxt)
    if terralib.islist(ss)  then  stmts:insertall(ss)
                            else  stmts:insert(ss)    end
  end
  return A.Block(stmts, self.srcinfo)
end

function AST.WhereFilter:typecheck(ctxt)
  local expr      = self.expr:typecheck(ctxt)
  if expr.type ~= T.error and expr.type ~= T.bool then
    ctxt:error(expr, "'where' expects a bool argument") end

  return A.WhereFilter(expr, self.srcinfo)
end

function AST.EmitStmt:typecheck(ctxt)
  local rec       = self.record:typecheck(ctxt)
  assert(self.dst:is_row(), 'INTERNAL: expect row type')
  local dsttyp    = self.dst:record_type()
  local rectyp    = rec.type

  -- build association mapping
  local mapping   = {}
  for i,rf in ipairs(rectyp.fields) do
    mapping[rf.name] = {
      rec         = rf,
      expr        = rec.exprs[i],
    }
  end
  local exprs     = newlist()
  for i,df in ipairs(dsttyp.fields) do
    local lookup      = mapping[df.name] or {}
    mapping[df.name]  = lookup
    lookup.dst        = df
    if lookup.rec then
      exprs:insert(lookup.expr)
      lookup.id       = #exprs
    end
  end

  -- now type-check
  for name,a in pairs(mapping) do
    if not a.rec then
      ctxt:error(self, "expected emitted record to contain field '"..
                       name.."'")
    elseif not a.dst then
      ctxt:error(a.expr, "table '"..self.dst:table():name().."' does not "..
                         "have a field named '"..name.."'")
    elseif a.rec.type ~= a.dst.type then
      -- try-to-coerce
      local expr  = do_coercion(exprs[a.id], a.dst.type, ctxt)
      exprs[a.id] = expr
    end
  end

  -- build a record expression in the correct field order
  rec             = A.RecordExpr(exprs, dsttyp, rec.srcinfo)
  return A.EmitStmt(rec, self.dst, self.srcinfo)
end

function AST.ExprStmt:typecheck(ctxt)
  local expr      = self.expr:typecheck(ctxt)

  -- This expression must resolve to a statement (via a quote)
  if expr.type == T.error then return newlist() -- silent fail
  elseif expr.type:is_internal() then
    local q = expr.type.value
    if is_quote(q) and q:is_statement() then
      local block = q._ast
      return block.stmts
    end
  end 

  -- otherwise, we failed
  ctxt:error(expr, "expected a statement, but got an expression instead")
  -- return a no-op statement
  return newlist()
end

function AST.ReturnStmt:typecheck(ctxt)
  local exprs     = typecheck_all(self.exprs, ctxt)
  assert(#exprs > 0, "INTERNAL: must have more than 0 exprs")

  local typ       = nil
  local prevtyp   = ctxt:get_return()

  -- Case 1: We still haven't inferred the return type, so simply do that.
  if prevtyp == T.error then
    typ           = exprs[1].type
    if #exprs > 1 then
      typ         = T.record( exprs:map(function(e) return e.type end) )
    end

  -- Case 2: We HAVE inferred the return type; so now we want to
  --         check consistency and insert coercive casts as needed
  else
    if #exprs == 1 then
      if exprs[1].type ~= T.error then
        exprs[1]  = do_coercion(exprs[1], prevtyp, ctxt)
      end
      typ         = exprs[1].type
    else
      local num_expect = (prevtyp:is_tuple() and #prevtyp.fields) or 1
      if #exprs ~= num_expect then
        ctxt:error(self, "expected "..tostring(num_expect)..
                         " expressions to return")
        typ       = T.error
      else
        -- Try to coerce each return expression;
        -- If there are errors anywhere, then mark the inferred type
        -- as error.  There's no point in reporting a less specific
        -- inconsistency.
        local es    = newlist()
        for i,e in ipairs(exprs) do
          if e.type ~= T.error then
            e       = do_coercion(e, prevtyp.fields[i].type, ctxt)
          end
          if e.type == T.error then typ = T.error end
          es:insert(e)
        end
        typ         = typ or prevtyp
      end
    end
  end

  -- package the result into a record if needed
  local expr        = exprs[1]
  if #exprs > 1 then
    expr            = A.RecordExpr(exprs, typ, self.srcinfo)
  end

  ctxt:add_return(typ, self)
  return A.ReturnStmt(expr, self.srcinfo)
end

---------------------------------------


function AST.IfCase:typecheck(ctxt)
  local cond      = self.cond:typecheck(ctxt)
  if cond.type ~= T.error and cond.type ~= T.bool then
    ctxt:error(cond, "expected boolean expression")
  end
  ctxt:enterblock()
  local body      = self.body:typecheck(ctxt)
  ctxt:leaveblock()
  return A.IfCase(cond, body, self.srcinfo)
end

function AST.IfStmt:typecheck(ctxt)
  local cases     = typecheck_all(self.cases, ctxt)
  ctxt:enterblock()
  local else_body = (self.else_body and self.else_body:typecheck(ctxt)) or nil
  ctxt:leaveblock()
  return A.IfStmt(cases, else_body, self.srcinfo)
end

function AST.DoStmt:typecheck(ctxt)
  ctxt:enterblock()
  local body      = self.body:typecheck(ctxt)
  ctxt:leaveblock()
  return body.stmts
end

function AST.ForLoop:typecheck(ctxt)
  local itername  = self.itername
  local itertype  = nil
  local lo        = self.lo:typecheck(ctxt)
  local hi        = self.hi:typecheck(ctxt)
  local stride    = (self.stride and self.stride:typecheck(ctxt)) or nil

  local has_err   = false
  if not (lo.type:is_primitive() and lo.type:is_integral()) then
    has_err       = true
    ctxt:error(lo, "expected integral expression") end
  if not (hi.type:is_primitive() and hi.type:is_integral()) then
    has_err       = true
    ctxt:error(hi, "expected integral expression") end
  itertype        = lo.type:join(hi.type)

  if stride then
    if not (stride.type:is_primitive() and stride.type:is_integral()) then
      has_err     = true
      ctxt:error(stride, "expected integral expression") end
    itertype      = itertype:join(stride.type)
  end

  -- Check if there was a type-join failure
  if itertype == T.error and not has_err then
    if lo.type ~= T.error and hi.type ~= T.error and
      (not stride or stride.type ~= T.error)
    then
      local stridemsg = (stride and ', '..tostring(stride.type)) or ''
      ctxt:error(self, "incompatible types on loop bound: "..
                       tostring(lo.type)..", "..tostring(hi.type)..stridemsg)
    end

  -- If not type-join failure, then coerce the expressions up to the
  -- common type
  else
    lo            = do_coercion(lo, itertype, ctxt)
    hi            = do_coercion(hi, itertype, ctxt)
    stride        = (stride and do_coercion(stride, itertype, ctxt)) or nil
  end

  ctxt:enterblock()
  ctxt:settype(itername, itertype)
  local body      = self.body:typecheck(ctxt)
  ctxt:leaveblock()
  return A.ForLoop(itername, lo, hi, stride, body, self.srcinfo)
end


---------------------------------------

function AST.Assignment:typecheck(ctxt)
  local me        = self
  local lvals     = self.lvals:map(function(lv)
                      lv    = lv:typecheck(ctxt)
                      if lv.type ~= T.error then
                        lv  = lv:lvalcheck(ctxt)
                      end
                      return lv
                    end)
  local rvals     = typecheck_all(self.rvals, ctxt)
  assert(#lvals > 0, "INTERNAL: must have more than 0 lvals")
  assert(#rvals > 0, "INTERNAL: must have more than 0 rvals")

  local function count_mismatch()
    ctxt:error(me, "expected assignment of "..(#lvals).." values on "..
                   "the left, but got "..(#rvals).." values on the right.")
  end

  -- check type-match and expand in an appropriate way
  local stmts     = newlist()
  if #lvals > 1 then -- n-way assignment or multi-arg function return-value

    -- Function Call Case:
    if #rvals == 1 then
      local fcall = rvals[1]
      if not A.FuncCall.check(fcall) then count_mismatch()
      else
        local nret = (fcall.type:is_tuple() and #fcall.type.fields) or 1
        if fcall.type == T.error then -- no-op
        elseif #lvals ~= nret then
          ctxt:error(self, "expected function call to return "..(#lvals)..
                           " values, but it returns "..nret)
        else
          -- now bind the function result
          local temp      = NewSymbol()
          stmts:insert( A.DeclStmt(temp, fcall.type, fcall, self.srcinfo) )
          local tvar      = A.Var(temp, fcall.type, self.srcinfo)
          -- and then unpack that result
          for i,f in ipairs(fcall.type.fields) do
            assert(f.type ~= T.error, 'INTERNAL: errors in rettype')
            if lvals[i].type ~= T.error then
              local read  = A.RecordRead(tvar, f.name, f.type, self.srcinfo)
              read        = do_coercion(read, lvals[i].type, ctxt)
              stmts:insert( A.Assignment(lvals[i], read, self.srcinfo) )
            end
          end
        end
      end

    -- N-way Assignment Case:
    elseif #lvals ~= #rvals then count_mismatch()
    else
      -- now bind the multiple rvals
      local has_err   = false
      local exprs     = newlist()
      for i,rv in ipairs(rvals) do
        if rv.type == T.error or lvals[i].type == T.error then
          has_err = true
        elseif not rv.type:is_value() then
          ctxt:error(rv, "expected value-typed expression")
          has_err     = true
        else
          rv          = do_coercion( rv, lvals[i].type, ctxt )
          exprs:insert( rv )
          if rv.type == T.error then has_err = true end
        end
      end

      -- form the rvals into a tuple and then unpack it
      if not has_err then
        local rectyp  = T.record( exprs:map(function(e) return e.type end) )
        local temp    = NewSymbol()
        local recexpr = A.RecordExpr( exprs, rectyp, self.srcinfo )
        stmts:insert(   A.DeclStmt(temp, rectyp, recexpr, self.srcinfo) )
        local tvar    = A.Var(temp, rectyp, self.srcinfo)
        -- and unpack the tuple then
        for i,f in ipairs(rectyp.fields) do
          local read  = A.RecordRead(tvar, f.name, f.type, self.srcinfo)
          stmts:insert( A.Assignment(lvals[i], read, self.srcinfo) )
        end
      end
    end

  -- Simple Assignment Case
  else
    assert(#lvals == 1)
    if #rvals ~= 1 then count_mismatch()
    elseif rvals[1].type == T.error or lvals[1].type == T.error then -- no-op
    elseif not rvals[1].type:is_value() then
      ctxt:error(rv, "expected value-typed expression")
    else
      local rval          = do_coercion( rvals[1], lvals[1].type, ctxt )
      stmts:insert(         A.Assignment(lvals[1], rval, self.srcinfo) )
    end
  end

  return stmts
end

function AST.Reduction:typecheck(ctxt)
  local lval      = self.lval:typecheck(ctxt)
  local rval      = self.rval:typecheck(ctxt)
  if lval.type ~= T.error then
    lval          = lval:lvalcheck(ctxt)
  end

  if lval.type ~= T.error and rval.type ~= T.error then
    if redop_token_set[self.op] then
      local has_err = false
      if not lval.type:is_numeric() then
        has_err   = true
        ctxt:error(lval, "expected numeric lvalue") end
      if not rval.type:is_numeric() then
        has_err   = true
        ctxt:error(rval, "expected numeric expression") end

      if not has_err then
        -- handle special case of tensor lvalue with scalar rvalue
        local ltyp  = lval.type
        if ltyp:is_tensor() and rval.type:is_primitive() then
          ltyp      = ltyp:basetype()
        end
        -- all cases: try to coerce left-hand side
        rval        = do_coercion(rval, ltyp, ctxt)
      end
      
    else
      INTERNAL_ERR('unexpected reduction operator: '..tostring(self.op))
    end
  end

  return A.Reduction(self.op, lval, rval, self.srcinfo)
end

function AST.DeclStmt:typecheck(ctxt)
  local me        = self
  local names     = self.names
  local typ       = self.type
  local rvals     = typecheck_all(self.rvals, ctxt)
  assert(#rvals > 0, "INTERNAL: must have more than 0 rvals")

  local function count_mismatch()
    ctxt:error(me, "expected definition of "..(#names).." values on "..
                   "the left, but got "..(#rvals).." values on the right.")
  end

  -- check type-match and expand in an appropriate way
  local stmts     = newlist()
  local bindtyps  = newlist()
  if #names > 1 then -- n-way assignment or multi-arg function return-value
    if typ then
      ctxt:error(self, "cannot annotate type on multi-variable declaration")

    -- Function Call Case:
    elseif #rvals == 1 and rvals[1].type ~= T.error then
      local fcall = rvals[1]
      if not A.FuncCall.check(fcall) then count_mismatch()
      else
        local nret = (fcall.type:is_tuple() and #fcall.type.fields) or 1
        if #names ~= nret then
          ctxt:error(self, "expected function call to return "..(#names)..
                           " values, but it returns "..nret)
        else
          -- now bind the function result
          local temp      = NewSymbol()
          stmts:insert( A.DeclStmt(temp, fcall.type, fcall, self.srcinfo) )
          local tvar      = A.Var(temp, fcall.type, self.srcinfo)
          -- and then unpack that result
          for i,f in ipairs(fcall.type.fields) do
            bindtyps[i]   = f.type
            local read    = A.RecordRead(tvar, f.name, f.type, self.srcinfo)
            stmts:insert(   A.DeclStmt(names[i], f.type, read, self.srcinfo) )
          end
        end
      end

    -- N-way Declaration Case:
    elseif #names ~= #rvals then count_mismatch()
    else
      -- now bind the multiple rvals
      local has_err   = false
      local exprs     = newlist()
      for i,rv in ipairs(rvals) do
        if rv.type == T.error then
          has_err = true
        elseif not rv.type:is_value() then
          ctxt:error(rv, "expected value type")
          has_err     = true
        else
          bindtyps[i] = rv.type
          exprs:insert( rv )
        end
      end

      -- form the rvals into a tuple and then unpack it
      if not has_err then
        local rectyp  = T.record( exprs:map(function(e) return e.type end) )
        local temp    = NewSymbol()
        local recexpr = A.RecordExpr( exprs, rectyp, self.srcinfo )
        stmts:insert(   A.DeclStmt(temp, rectyp, recexpr, self.srcinfo) )
        local tvar    = A.Var(temp, rectyp, self.srcinfo)
        -- and unpack the tuple then
        for i,f in ipairs(rectyp.fields) do
          local read  = A.RecordRead(tvar, f.name, f.type, self.srcinfo)
          stmts:insert( A.DeclStmt(names[i], read.type, read, self.srcinfo) )
        end
      end
    end

  -- Simple Declaration Case
  else
    assert(#names == 1)
    local rv = rvals[1]
    if typ then
      if typ == T.error then  -- noop
      elseif rv.type == T.error then -- noop
      elseif not rv.type:is_value() then
        ctxt:error(rv, "expected value type")
      else
        rv            = do_coercion(rv, typ, ctxt)
      end
    else
      typ             = rv.type
    end

    bindtyps[1]       = typ
    stmts:insert(       A.DeclStmt(names[1], typ, rv, self.srcinfo) )
  end

  -- bind new variables to types
  for i,n in ipairs(names) do
    ctxt:settype(n, bindtyps[i] or T.error)
  end

  return stmts
end


-------------------------------------------------------------------------------
--[[                           Expression Rules                            ]]--
-------------------------------------------------------------------------------

function AST.NumLiteral:typecheck(ctxt)
  return A.NumLiteral(self.value, self.type, self.srcinfo)
end
function AST.BoolLiteral:typecheck(ctxt)
  return A.BoolLiteral(self.value, T.bool, self.srcinfo)
end

-- What can this possibly be?
-- We can be prescriptive!
--    * String      -- convert to string object
--    * Macro       -- return as lua object to be CALLED
--    * Quote       -- if EXPR, unpack; if STMT, return lua obj for ExprStmt
--    * Function    -- return as lua object to be CALLED
--    * BuiltIn     -- return as lua object to be CALLED
--    * Gong Type   -- return as lua object to be CALLED
function AST.LuaObj:typecheck(ctxt)
  local obj       = self.obj
  if not is_gong_obj(obj) then
    ctxt:error(self, gong_obj_err_msg)
    return A.LuaObj(T.error, self.srcinfo)
  elseif is_quote(obj) and not obj:is_statement() then
    local expr    = obj._ast
    return expr
  else
    return A.LuaObj(T.internal(obj), self.srcinfo)
  end
end

function AST.Var:typecheck(ctxt)
  local typ       = assert(ctxt:gettype( self.name ),
                           'INTERNAL: bad variable referent')
  local newvar    = A.Var(self.name, typ, self.srcinfo)
  if typ:is_unknown_tensorindex() then -- make sure this gets patched
    ctxt:set_tensorindex(self.name, typ, newvar)
  end
  return newvar
end


---------------------------------------

function AST.Call:typecheck(ctxt)
  local base      = self.base:typecheck(ctxt)
  local args      = typecheck_all(self.args, ctxt)

  -- determine type of call construct
  local calltype  = T.error
  local obj       = nil
  if base.type == T.error then --no-op
  elseif not base.type:is_internal() then
    ctxt:error(base, "expected Macro, Function, or Type-cast to call")
  else
    local obj = base.type.value

    -- Explicit Type-Casting
    if     is_type(obj) then
      calltype    = obj
      if #args ~= 1 then
        ctxt:error(self, "expected exactly one argument to type-cast")
      else
        local newtyp    = obj
        local prevtyp   = args[1].type
        if prevtyp == T.error then -- no-op
        elseif prevtyp:is_tensorindex() then -- special case
          if not newtyp:is_numeric() or not newtyp:is_primitive() then
            ctxt:error(base, "cannot cast tensor index to "..
                             "non-numeric type: "..tostring(newtyp))
          else
            return A.Cast(args[1], newtyp, self.srcinfo)
          end
        elseif not (prevtyp:is_pure_tensor() or prevtyp:is_primitive()) or
               not (newtyp:is_pure_tensor()  or newtyp:is_primitive())
        then
          ctxt:error(self, "can only cast between primitives and tensors "..
                           "of primitives, not the following:\n    "..
                           tostring(prevtyp).."\n to "..tostring(newtyp))
        elseif prevtyp:has_rows() or newtyp:has_rows() then
          ctxt:error(self, "cannot cast between row-valued types")
        else
          if prevtyp:is_tensor() and newtyp:is_primitive() then
            ctxt:error(self, "cannot cast tensor to primitive")
          elseif prevtyp:is_primitive() and newtyp:is_tensor() then
            ctxt:error(self, "cannot cast primitive to tensor")
          elseif not prevtyp:dims_match(newtyp) then
            ctxt:error(self, "cannot cast tensors of mismatched dimensions")
          else
            return A.Cast(args[1], newtyp, self.srcinfo)
          end
        end
      end

    -- Macro Execution
    elseif is_macro(obj) then
      -- short-circuit if we have errors in the arguments...
      -- quote conversion
      local arg_errs  = false
      local qs  = args:map(function(a)
        if     a.type == T.error then arg_errs = true
        elseif a.type:is_internal() then
          return a.type.value
        else -- return quoted expression
          return Macro.NewQuote(a, a.srcinfo, false)
        end
      end)
      if arg_errs then -- no-op
      else
        -- TODO: more safety on errors during macro execution
        local quot = obj._clbk(unpack(qs))
        if not is_quote(quot) then
          ctxt:error(self, 'expected macro to return quoted gong code')
        else
          -- unpack quote
          if quot:is_statement() then
            return A.LuaObj(T.internal(quot), self.srcinfo)
          else
            return quot._ast
          end
        end
      end

    -- Function Argument Typechecking
    elseif is_function(obj) then
      calltype        = obj:rettype()
      -- check arguments
      local argtypes  = obj:argtypes()
      if #argtypes ~= #args then
        ctxt:error(self, "expected "..(#argtypes).." arguments, but got "..
                         (#args).." arguments")
      else
        local has_err = false
        for i,t in ipairs(argtypes) do
          local at = args[i].type
          if at == T.error then has_err = true
          else
            args[i]   = do_coercion(args[i], t, ctxt)
          end
        end
        return A.FuncCall(obj, args, calltype, self.srcinfo)
      end

    -- Built-In Typechecking
    elseif is_builtin(obj) then
      local typ, errs   = obj._typecheck(unpack(args))
      if typ == T.error and #errs > 0 then
        for _,e in ipairs(errs) do ctxt:error(self, e) end
      else
        return A.BuiltInCall(obj, args, typ, self.srcinfo)
      end
    else
      ctxt:error(base, "expected Macro, Function, or Type-cast to call")
    end
  end

  return A.Var(NewSymbol(), calltype, self.srcinfo)
end

---------------------------------------

function AST.TensorMap:typecheck(ctxt)
  ctxt:enterblock()
  -- introduce index variables
  for _,n in ipairs(self.names) do
    ctxt:set_tensorindex(n, T.tensorindex()) -- uninferred type
  end
  -- typecheck the expression
  local expr      = self.expr:typecheck(ctxt)
  -- now read out the inferred index data
  local has_err   = false
  local idxtypes  = newlist()
  local dims      = newlist()
  for i,n in ipairs(self.names) do
    local t       = ctxt:gettype(n)
    if t:is_unknown_tensorindex() then
      has_err     = true
      dims:insert(1)
    else
      dims:insert(t.range)
    end
    idxtypes:insert(t)
  end
  ctxt:leaveblock()

  local mtyp      = T.error
  if not has_err and expr.type ~= T.error then
    if not expr.type:is_value() then
      ctxt:error(expr, "expected value-type expression in map")
    else
      mtyp        = T.tensor(expr.type, unpack(dims))
    end
  end

  return A.TensorMap(self.names, idxtypes, expr, mtyp, self.srcinfo)
end

function AST.TensorFold:typecheck(ctxt)
  ctxt:enterblock()
  -- introduce index variables
  for _,n in ipairs(self.names) do
    ctxt:set_tensorindex(n, T.tensorindex()) -- uninferred type
  end
  -- typecheck the expression
  local expr      = self.expr:typecheck(ctxt)
  -- now read out the inferred index data
  local has_err   = false
  local idxtypes  = newlist()
  for i,n in ipairs(self.names) do
    local t       = ctxt:gettype(n)
    if t:is_unknown_tensorindex() then has_err = true end
    idxtypes:insert(t)
  end
  ctxt:leaveblock()

  local ftyp      = T.error
  if not has_err and expr.type ~= T.error then
    if not expr.type:is_value() then
      ctxt:error(expr, "expected value-type expression in fold")
    elseif redop_token_set[self.op] then
      if not expr.type:is_numeric() then
        ctxt:error(expr, "expected numeric-valued expression to fold")
      else
        ftyp      = expr.type
      end
    else
      INTERNAL_ERR('unexpected fold reduction operator: '..self.op)
    end
  end

  return A.TensorFold(self.op, self.names, idxtypes,
                      expr, ftyp, self.srcinfo)
end

function AST.Lookup:typecheck(ctxt)
  local base      = self.base:typecheck(ctxt)
  local args      = typecheck_all( self.args, ctxt )
  local errtyp    = T.error

  if base.type == T.error then -- no-op

  -- Tensor Indexing
  elseif base.type:is_tensor() then
    local dims    = base.type.dims
    errtyp        = base.type:basetype()
    if #args ~= #dims then
      ctxt:error(self, "expected "..(#dims).." arguments, but got "..(#args))
    else
      local has_err = false
      for i,d in ipairs(dims) do
        local at  = args[i].type
        if at == T.error then
          has_err = true
        elseif at:is_unknown_tensorindex() then -- infer
          assert(A.Var.check(args[i]),
                 "INTERNAL: expected unknown indices as basic variables")
          ctxt:set_tensorindex(args[i].name, T.tensorindex(d), args[i])
        elseif at:is_tensorindex() then -- check
          if at.range ~= d then
            ctxt:error(args[i], "expected index ranging from 0 to "..(d-1)..
                                " but got index ranging to "..(at.range-1))
            has_err = true
          end
        elseif at:is_integral() and at:is_primitive() then -- all-good
        else
          ctxt:error("cannot index with expression of type: "..tostring(at))
          has_err = true
        end
      end
      if not has_err then
        -- decide whether this is just indexing a tensor object, or
        -- if we can fold this indexing into a memory-read operation
        local typ = base.type:basetype()
        if A.TableRead.check(base) then
          local path    = base.path:copy()
          path:insert( A.PathIndex(args, self.srcinfo) )
          return A.TableRead(base.base, path, typ, self.srcinfo)
        else
          return A.TensorIndex(base, args, typ, self.srcinfo)
        end
      end
    end

  -- Record Indexing
  -- Data-Table Indexing
  elseif base.type:is_record() or base.type:is_row() then
    local tname = (base.type:is_record() and 'record') or 'row'
    if #args ~= 1 then
      ctxt:error(self, "expected exactly one argument to index a "..tname)
    else
      local at = args[1].type
      if at == T.error then -- fall-through
      elseif at:is_internal() and type(at.value) == 'string' then
        if base.type:is_record() then
          local i,f   = base.type:lookup(at.value)
          if not i then
            ctxt:error(self, "could not find field '"..at.value.."' in "..
                             "record of type: "..tostring(base.type))
          else
            if A.TableRead.check(base) then
              local path    = base.path:copy()
              path:insert( A.PathField(f.name, self.srcinfo) )
              return A.TableRead(base.base, path, f.type, self.srcinfo)
            else
              return A.RecordRead(base, f.name, f.type, self.srcinfo)
            end
          end
        else -- base.type is row
          local tbl   = base.type:table()
          local f     = tbl:fields(at.value)
          if not f then
            ctxt:error( self, "could not find field '"..at.value.."' in "..
                              "row from table: "..tbl:name() )
          else
            local path = newlist{ A.PathField(at.value, self.srcinfo) }
            return A.TableRead(base, path, f:type(), self.srcinfo)
          end
        end
      else
        ctxt:error(args[1], 'expected a string argument')
      end
    end

  else
    ctxt:error(base, "cannot index an expression of this type: "..
                     tostring(base.type))
  end

  return A.Var( NewSymbol(), errtyp, self.srcinfo )
end

---------------------------------------

function AST.RecordExpr:typecheck(ctxt)
  local exprs     = typecheck_all( self.exprs, ctxt )
  assert(#self.names == #self.exprs,
         "INTERNAL: parser should have ensured matching numbers of "..
         "fields and names")
  assert(#self.names > 0, "INTERNAL")

  -- assemble record fields
  local has_err   = false
  local repeats   = {}
  local fs        = newlist()
  for i,e in ipairs(exprs) do
    if     e.type == T.error then
      has_err = true
    elseif not e.type:is_value() then
      ctxt:error(e, "expected value type")
      has_err = true
    elseif repeats[ self.names[i] ] then
      ctxt:error(e, "cannot use repeated field names: '"..self.names[i].."'")
      has_err = true
    else
      fs:insert { self.names[i], e.type }
    end
    repeats[ self.names[i] ] = true
  end

  local typ       = T.error
  if not has_err then
    typ           = T.record(fs)
  end

  return A.RecordExpr(exprs, typ, self.srcinfo)
end

function AST.ListExpr:typecheck(ctxt)
  local exprs     = typecheck_all( self.exprs, ctxt )
  assert(#exprs > 0, "INTERNAL")

  -- figure out common type
  local typ       = exprs[1].type
  for i,e in ipairs(exprs) do
    local ntyp    = typ:join(e.type)
    if typ ~= T.error and e.type ~= T.error and ntyp == T.error then
      ctxt:error(e, 'cannot form a vector/tensor of inconsistently typed '..
                    'entries.  Expected '..tostring(typ)..' but got '..
                    tostring(e.type))
    end
    typ           = ntyp
  end

  if typ == T.error then -- noop
  elseif not typ:is_value() then
    ctxt:error(self, "cannot form a vector of non-value expressions")
    typ           = T.error
  elseif typ ~= T.error then
    for i=1,#exprs do
      exprs[i]    = do_coercion(exprs[i], typ, ctxt)
    end
    typ           = T.vector(typ, #exprs)
  end

  return A.ListExpr(exprs, typ, self.srcinfo)
end

---------------------------------------

local function do_bin_coercion(self, lhs, rhs, ctxt, booloverride)
  local typ       = lhs.type:join(rhs.type)
  if typ == T.error then
    ctxt:error(self, "could not find common type to coerce operands to; "..
                     "lhs/rhs types: "..tostring(lhs.type)..
                                 " / "..tostring(rhs.type))
    return A.BinaryOp(self.op, lhs, rhs, T.error, self.srcinfo)
  end

  lhs             = do_coercion(lhs, typ, ctxt)
  rhs             = do_coercion(rhs, typ, ctxt)

  return A.BinaryOp( self.op, lhs, rhs,
                     (booloverride and T.bool) or typ,
                     self.srcinfo )
end

local function do_scalartensor_bin_coercion(self, lhs, rhs, ctxt)
  assert(lhs.type:is_tensor() ~= rhs.type:is_tensor())
  local lbtyp     = (lhs.type:is_tensor() and lhs.type:basetype()) or lhs.type
  local rbtyp     = (rhs.type:is_tensor() and rhs.type:basetype()) or rhs.type
  local btyp      = lbtyp:join(rbtyp)
  if btyp == T.error then
    ctxt:error(self, "could not find common type to coerce operands to; "..
                     "lhs/rhs types: "..tostring(lhs.type)..
                                 " / "..tostring(rhs.type))
    return A.BinaryOp(self.op, lhs, rhs, T.error, self.srcinfo)
  end

  local ltyp      = ( lhs.type:is_tensor() and
                      T.tensor(btyp, unpack(lhs.type.dims)) ) or btyp
  local rtyp      = ( rhs.type:is_tensor() and
                      T.tensor(btyp, unpack(rhs.type.dims)) ) or btyp
  lhs             = do_coercion(lhs, ltyp, ctxt)
  rhs             = do_coercion(rhs, rtyp, ctxt)

  return A.BinaryOp( self.op, lhs, rhs,
                     (ltyp:is_tensor() and ltyp) or rtyp,
                     self.srcinfo )
end

function AST.BinaryOp:typecheck(ctxt)
  local me        = self
  local op        = self.op
  local lhs       = self.lhs:typecheck(ctxt)
  local rhs       = self.rhs:typecheck(ctxt)

  local function err(prefix)
    ctxt:error(me, prefix.."; lhs/rhs types: "..
                     tostring(lhs.type)..' / '..tostring(rhs.type))
    return A.BinaryOp(op, lhs, rhs, T.error, me.srcinfo)
  end
  if lhs.type == T.error or rhs.type == T.error then
    return A.BinaryOp(op, lhs, rhs, T.error, self.srcinfo)
  
  elseif not lhs.type:is_value() or not rhs.type:is_value() then
    return err("cannot combine non-values using '"..op.."'")

  -- breakdown by operator, type-dims, basetypes

  elseif op == 'and' or op == 'or' then
    if not lhs.type:is_logical() or not rhs.type:is_logical() then
      return err("expected boolean operands")
    elseif not lhs.type:dims_match(rhs.type) then
      return err("dimensions don't match")
    else
      return A.BinaryOp(op, lhs, rhs, lhs.type, self.srcinfo)
    end

  elseif op == '<=' or op == '>=' or op == '<' or op == '>' then
    if lhs.type:is_tensor() or rhs.type:is_tensor() then
      return err("expected scalar operands")
    elseif not lhs.type:is_numeric() or not rhs.type:is_numeric() then
      return err("expected numeric operands")
    else
      return do_bin_coercion(self, lhs, rhs, ctxt, true) -- bool-override
    end

  elseif op == '==' or op == '~=' then
    if not lhs.type:dims_match(rhs.type) then
      return err("dimensions don't match")
    else
      return do_bin_coercion(self, lhs, rhs, ctxt, true) -- bool-override
    end

  elseif op == '+' or op == '-' then
    if not lhs.type:is_numeric() or not rhs.type:is_numeric() then
      return err("expected numeric operands")
    elseif not lhs.type:dims_match(rhs.type) then
      return err("dimensions don't match")
    else
      return do_bin_coercion(self, lhs, rhs, ctxt)
    end

  elseif op == '*' then
    if not lhs.type:is_numeric() or not rhs.type:is_numeric() then
      return err("expected numeric operands")
    elseif lhs.type:is_tensor() == rhs.type:is_tensor() then
      if lhs.type:is_tensor() then
        return err("cannot take product of two tensors; ambiguous meaning")
      else
        return do_bin_coercion(self, lhs, rhs, ctxt)
      end
    else
      return do_scalartensor_bin_coercion(self, lhs, rhs, ctxt)
    end

  elseif op == '/' then
    if not lhs.type:is_numeric() or not rhs.type:is_numeric() then
      return err("expected numeric operands")
    elseif rhs.type:is_tensor() then
      return err("cannot divide by a tensor")
    elseif lhs.type:is_tensor() then
      return do_scalartensor_bin_coercion(self, lhs, rhs, ctxt)
    else
      return do_bin_coercion(self, lhs, rhs, ctxt)
    end

  elseif op == '%' then
    if not lhs.type:is_integral() or not rhs.type:is_integral() then
      return err("expected integral operands")
    elseif lhs.type:is_tensor() or rhs.type:is_tensor() then
      return err("expected scalar operands")
    else
      return do_bin_coercion(self, lhs, rhs, ctxt)
    end

  end
  INTERNAL_ERR(self, "unrecognized binary operator '"..op.."'")
end

function AST.UnaryOp:typecheck(ctxt)
  local op        = self.op
  local expr      = self.expr:typecheck(ctxt)
  local typ       = expr.type

  if typ == T.error then -- no-op
  elseif op == 'not' then
    if not expr.type:is_logical() then
      ctxt:error(self, "unary 'not' expects a boolean operand") end
  elseif op == '-' then
    if not expr.type:is_signed() then
      ctxt:error(self, "unary '-' expects a signed numeric operand") end
  else
    INTERNAL_ERR("unrecognized unary operator '"..op.."'")
  end

  return A.UnaryOp(op, expr, typ, self.srcinfo)
end

function AST.TernaryExpr:typecheck(ctxt)
  local cond      = self.cond:typecheck(ctxt)
  local lhs       = self.lhs:typecheck(ctxt)
  local rhs       = self.rhs:typecheck(ctxt)

  if cond.type ~= T.error and cond.type ~= T.bool then
    ctxt:error(cond, "expected boolean type expression")
  end

  local typ       = lhs.type:join(rhs.type)
  if lhs.type == T.error or rhs.type == T.error then -- no-op
  elseif typ == T.error then
    ctxt:error(self, "Inconsistent types returned by branches of "..
                     "ternary expression: "..tostring(lhs.type)..
                     " vs. "..tostring(rhs.type))
  else
    lhs           = do_coercion(lhs, typ, ctxt)
    rhs           = do_coercion(rhs, typ, ctxt)
  end

  return A.TernaryExpr(cond, lhs, rhs, typ, self.srcinfo)
end

function AST.LetExpr:typecheck(ctxt)
  ctxt:enterblock()
  local block     = self.block:typecheck(ctxt)
  local expr      = self.expr:typecheck(ctxt)
  ctxt:leaveblock()

  return A.LetExpr(block, expr, expr.type, self.srcinfo)
end


-------------------------------------------------------------------------------
--[[                       Expression L-Value Rules                        ]]--
-------------------------------------------------------------------------------

-- What is allowed: variables, reads (convert to writes), and deconstructors
function A.Var:lvalcheck(ctxt)
  return self
end

-- Table Reads get converted into Table Writes
-- otherwise, we leave things as is
function A.TableRead:lvalcheck(ctxt)
  -- base is a row-type value, which needn't further be an l-value
  return A.TableWrite(self.base, self.path, self.type, self.srcinfo)
end

function A.TensorIndex:lvalcheck(ctxt)
  local base      = self.base:lvalcheck(ctxt)
  return A.TensorIndex(base, self.args, self.type, self.srcinfo)
end

function A.RecordRead:lvalcheck(ctxt)
  local base      = self.base:lvalcheck(ctxt)
  return A.RecordRead(base, self.arg, self.type, self.srcinfo)
end


-- What isn't allowed
function A.LuaObj:lvalcheck(ctxt)
  ctxt:error(self, 'expected lvalue, not expression of type '..
                   tostring(self.type))
  return self
end

function A.NumLiteral:lvalcheck(ctxt)
  ctxt:error(self, 'literals are not lvalues')
  return self
end
function A.BoolLiteral:lvalcheck(ctxt)
  ctxt:error(self, 'literals are not lvalues')
  return self
end

function A.RecordExpr:lvalcheck(ctxt)
  ctxt:error(self, "record constructors not allowed in lvalues")
  return self
end
function A.ListExpr:lvalcheck(ctxt)
  ctxt:error(self, "vector/tensor constructors not allowed in lvalues")
  return self
end

function A.BinaryOp:lvalcheck(ctxt)
  ctxt:error(self, "operators and arithmetic not allowed in lvalues")
  return self
end
function A.UnaryOp:lvalcheck(ctxt)
  ctxt:error(self, "operators and arithmetic not allowed in lvalues")
  return self
end
function A.TernaryExpr:lvalcheck(ctxt)
  ctxt:error(self, "operators and arithmetic not allowed in lvalues")
  return self
end
function A.LetExpr:lvalcheck(ctxt)
  ctxt:error(self, "let expressions are not allowed in lvalues")
  return self
end

function A.Cast:lvalcheck(ctxt)
  ctxt:error(self, "type-casts are not allowed in lvalues")
  return self
end
function A.BuiltInCall:lvalcheck(ctxt)
  ctxt:error(self, "calls are not allowed in lvalues")
  return self
end
function A.FuncCall:lvalcheck(ctxt)
  ctxt:error(self, "calls are not allowed in lvalues")
  return self
end
function A.TensorMap:lvalcheck(ctxt)
  ctxt:error(self, "tensor maps are not allowed in lvalues")
  return self
end
function A.TensorFold:lvalcheck(ctxt)
  ctxt:error(self, "tensor folds are not allowed in lvalues")
  return self
end


