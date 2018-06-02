import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.specializer"] = Exports

-------------------------------------------------------------------------------
-- Specialize is a compiler pass that substitutes in as many up-values as
-- possible, such that a proper closure can be formed
-- It should always be run at declaration time, not deferred till
-- execution/compilation.
--    Specialization also converts parsed names into symbols.
-------------------------------------------------------------------------------

local Parser      = require('gong.src.parser')
local AST         = Parser.AST
local T           = require 'gong.src.types'
local is_type     = T.is_type
local Util        = require 'gong.src.util'
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
local is_global   = Global.is_global
local is_function = Functions.is_function
local is_builtin  = Functions.is_builtin

local newlist   = terralib.newlist

-------------------------------------------------------------------------------
--                                    AST                                    --
-------------------------------------------------------------------------------

local binop_token_set       = Parser.binop_token_set
local unop_token_set        = Parser.unop_token_set
local redop_token_set       = Parser.redop_token_set

Exports.binop_token_set     = binop_token_set
Exports.unop_token_set      = unop_token_set
Exports.redop_token_set     = redop_token_set

local ADT A
  -- Top Level Forms
  QuoteExpr   = { expr    : Expr,             srcinfo : SrcInfo }
  QuoteStmt   = { block   : Block,            srcinfo : SrcInfo }
  Function    = { name    : string,
                  args    : ArgDecl*,
                  rettype : Type?,
                  body    : Block,            srcinfo : SrcInfo }
  Join        = { name    : string,
                  args    : ArgDecl*,
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
        | MergeStmt   { dst       : Type,
                        new_rec   : RecordExpr?,
                        up_name   : Symbol,   up_body   : Block,
                        rm_name   : Symbol?,  rm_body   : Block? }
        | ExprStmt    { expr      : Expr }
        | ReturnStmt  { exprs     : Expr* }
    -- Basic Structural Statements
        | IfStmt      { cases     : IfCase*,  else_body : Block? }
        | DoStmt      { body      : Block }
    -- Binding & Assignment
        | Assignment  { lvals     : Expr*,    rvals     : Expr* }
        | Reduction   { op        : redop,
                        lval      : Expr,     rval      : Expr }
        | DeclStmt    { names     : Symbol*,  type      : Type?,
                        rvals     : Expr* }
    -- Looping
        | ForLoop     { itername  : Symbol,
                        lo        : Expr,     hi        : Expr,
                        stride    : Expr?,
                        body      : Block }
        attributes {                          srcinfo : SrcInfo }

  -- Expressions
  Expr =
    -- Literals and other Atoms
          Var         { name  : Symbol }
        | LuaObj      { obj   : any }
        | Global      { obj   : Glob }
        | NumLiteral  { value : number,       type      : Type }
        | BoolLiteral { value : boolean }
    -- Data Constructors
        | RecordExpr  { names : id_str*,      exprs     : Expr* }
        | ListExpr    { exprs : Expr* }
    -- Data Destructors
        -- lookup is base[a1,...]
        | Lookup      { base  : Expr,         args      : Expr* }
    -- Basic building up of conditions and terms
        | BinaryOp    { op    : binop,
                        lhs   : Expr,         rhs       : Expr }
        | UnaryOp     { op    : unop,         expr      : Expr }
        | TernaryExpr { cond  : Expr,
                        lhs   : Expr,         rhs       : Expr }
        | LetExpr     { block : Block,        expr      : Expr }
    -- Special forms
        -- call is base(a1,...)
        | Call        { base  : Expr,         args      : Expr* }
        | KeepExpr    { arg   : Expr }
        | TensorMap   { names : Symbol*,      expr      : Expr  }
        | TensorFold  { op    : redop,
                        names : Symbol*,      expr      : Expr  }
        attributes {                          srcinfo : SrcInfo }

  extern binop  function(obj) return binop_token_set[obj] end
  extern unop   function(obj) return unop_token_set[obj]  end
  extern redop  function(obj) return redop_token_set[obj] end

  extern Type     is_type
  extern Symbol   is_symbol
  extern Glob     is_global
  extern any      function(obj) return true end

  extern id_str   is_id_str
  extern SrcInfo  function(obj) return SrcInfo.check(obj) end
end
Exports.AST = A

-------------------------------------------------------------------------------
--[[                          Context Definition                           ]]--
-------------------------------------------------------------------------------
local Context = {}
Context.__index = Context

function NewContext(luaenv)
  local ctxt = setmetatable({
    env       = terralib.newenvironment(luaenv),
    _luaenv   = luaenv,
    _env_stk  = newlist(),
    diag      = terralib.newdiagnostics(),
  }, Context)
  return ctxt
end
function Context:pushenv()
  self._env_stk:insert(self.env)
  self.env = terralib.newenvironment(self._luaenv)
end
function Context:in_merge_remove()
  return #self._env_stk > 0
end
function Context:popenv()
  self.env = self._env_stk:remove()
end
function Context:localenv()
  return self.env:localenv()
end
function Context:luaenv()
  return self.env:luaenv()
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
  self.diag:finishandabortiferrors("Errors during specialization", lvl+1)
end


-------------------------------------------------------------------------------
--[[                   Entry Point and Support Functions                   ]]--
-------------------------------------------------------------------------------

function Exports.specialize(input_ast, luaenv)
  local ctxt    = NewContext(luaenv)

  ctxt:enterblock()
  local output_ast = input_ast:specialize(ctxt)
  ctxt:leaveblock()
  ctxt:finishandabortiferrors(1)

  return output_ast
end


local function luaeval(expr, anchor, ctxt, default)
  local status, val = pcall(function()
    return expr(ctxt:luaenv())
  end)
  if not status then
    ctxt:error(anchor, "Error evaluating Lua expression")
    return default
  else
    return val
  end
end

local function eval_type_annotation(annotation, anchor, ctxt)
  local typ = luaeval(annotation, anchor, ctxt, T.error)

  -- promote Gong tables into row types
  if Schemata.is_table(typ) then
    typ = T.row(typ)

  -- promote raw lists of types into record-tuples
  -- This is especially valuable for return-type function annotations
  elseif terralib.israwlist(typ) then
    local istuple = true
    local tlist   = newlist()
    for i,t in ipairs(typ) do
      if not T.is_type(t) then istuple = false; break end
    end
    if istuple then
      typ = T.record(typ)
    end
  end

  -- handle other promotions of non-types into types

  if not T.is_type(typ) then
    ctxt:error(anchor, "Expected type but found "..type(typ))
    typ = T.error
  end
  return typ
end

local function introsym(namestr, ctxt)
  local namesym = NewSymbol(namestr)
  ctxt:localenv()[namestr] = namesym
  return namesym
end

local function specialize_all(xs, ctxt)
  return xs:map(function(x) return x:specialize(ctxt) end)
end

-------------------------------------------------------------------------------
--[[                            Top-Level Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Quote:specialize(ctxt)
  local body, expr = nil, nil

  if self.body then body = self.body:specialize(ctxt) end
  if self.expr then expr = self.expr:specialize(ctxt) end

  assert(expr or body,
         'expected a quote to contain some statement or expression')
  if expr and body then
    return A.QuoteExpr( A.LetExpr(body, expr, self.srcinfo), self.srcinfo )
  elseif expr then
    return A.QuoteExpr( expr, self.srcinfo )
  else
    return A.QuoteStmt( body, self.srcinfo )
  end
end

function AST.Function:specialize(ctxt)
  local args      = specialize_all(self.args, ctxt)
  local rettype   = ( self.rettype and
                      eval_type_annotation(self.rettype, self, ctxt) ) or nil
  local body      = self.body:specialize(ctxt)

  return A.Function(self.name, args, rettype, body, self.srcinfo)
end

function AST.Join:specialize(ctxt)
  local args      = specialize_all(self.args, ctxt)
  local filter    = self.filter:specialize(ctxt)
  local doblock   = self.doblock:specialize(ctxt)

  return A.Join(self.name, args, filter, doblock, self.srcinfo)
end

function AST.ArgDecl:specialize(ctxt)
  local name      = introsym(self.name, ctxt)
  local typ       = eval_type_annotation(self.type, self, ctxt)
  return A.ArgDecl(name, typ, self.srcinfo)
end


-------------------------------------------------------------------------------
--[[                            Statement Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Block:specialize(ctxt)
  local stmts     = specialize_all( self.stmts, ctxt )
  return A.Block(stmts, self.srcinfo)
end

----------------------------------------

function AST.WhereFilter:specialize(ctxt)
  local expr      = self.expr:specialize(ctxt)
  return A.WhereFilter(expr, self.srcinfo)
end

function AST.EmitStmt:specialize(ctxt)
  local record    = self.record:specialize(ctxt)
  local dst       = luaeval(self.dst_table, self, ctxt)
  if not Schemata.is_table(dst) then
    ctxt:error(self, "expected a Gong Table to emit into")
    dst           = T.error
  else
    dst           = T.row(dst)
  end

  return A.EmitStmt(record, dst, self.srcinfo)
end

function AST.MergeStmt:specialize(ctxt)
  local dst       = luaeval(self.dst_table, self, ctxt)
  if not Schemata.is_table(dst) then
    ctxt:error(self, "expected a Gong Table to emit into")
    dst           = T.error
  else
    dst           = T.row(dst)
  end

  -- new?
  local new_rec   = nil
  if self.new_rec then
    new_rec       = self.new_rec:specialize(ctxt)
  end

  -- update:
  ctxt:enterblock()
  local up_name   = introsym(self.up_name, ctxt)
  local up_body   = self.up_body:specialize(ctxt)
  ctxt:leaveblock()

  -- remove? (needs a new context)
  local rm_name, rm_body = nil, nil
  if self.rm_name then
    ctxt:pushenv()
    rm_name       = introsym(self.rm_name, ctxt)
    rm_body       = self.rm_body:specialize(ctxt)
    ctxt:popenv()
  end

  return A.MergeStmt(dst, new_rec, up_name, up_body,
                                   rm_name, rm_body, self.srcinfo)
end

function AST.ExprStmt:specialize(ctxt)
  local expr      = self.expr:specialize(ctxt)
  return A.ExprStmt(expr, self.srcinfo)
end

function AST.ReturnStmt:specialize(ctxt)
  local exprs     = specialize_all( self.exprs, ctxt )
  return A.ReturnStmt(exprs, self.srcinfo)
end

----------------------------------------

function AST.IfCase:specialize(ctxt)
  local cond      = self.cond:specialize(ctxt)
  ctxt:enterblock()
  local body      = self.body:specialize(ctxt)
  ctxt:leaveblock()
  return A.IfCase(cond, body, self.srcinfo)
end

function AST.IfStmt:specialize(ctxt)
  local cases     = specialize_all( self.cases, ctxt )
  ctxt:enterblock()
  local else_body = ( self.else_body and self.else_body:specialize(ctxt) )
                                      or nil
  ctxt:leaveblock()
  return A.IfStmt(cases, else_body, self.srcinfo)
end

function AST.DoStmt:specialize(ctxt)
  ctxt:enterblock()
  local body      = self.body:specialize(ctxt)
  ctxt:leaveblock()
  return A.DoStmt(body, self.srcinfo)
end

----------------------------------------

function AST.Assignment:specialize(ctxt)
  local lvals     = specialize_all( self.lvals, ctxt )
  local rvals     = specialize_all( self.rvals, ctxt )
  return A.Assignment(lvals, rvals, self.srcinfo)
end

function AST.Reduction:specialize(ctxt)
  local lval      = self.lval:specialize(ctxt)
  local rval      = self.rval:specialize(ctxt)
  return A.Reduction(self.op, lval, rval, self.srcinfo)
end

function AST.DeclStmt:specialize(ctxt)
  local rvals     = specialize_all(self.rvals, ctxt)

  local names     = newlist()
  for _,nm in ipairs(self.names) do
    names:insert( introsym(nm, ctxt) ) end
  local typ       = ( self.type and
                      eval_type_annotation(self.type, self, ctxt) ) or nil

  return A.DeclStmt(names, typ, rvals, self.srcinfo)
end

----------------------------------------

function AST.ForLoop:specialize(ctxt)
  local lo        = self.lo:specialize(ctxt)
  local hi        = self.hi:specialize(ctxt)
  local stride    = ( self.stride and self.stride:specialize(ctxt) ) or nil
  ctxt:enterblock()
  local itername  = introsym(self.itername, ctxt)
  local body      = self.body:specialize(ctxt)
  ctxt:leaveblock()

  return A.ForLoop(itername, lo, hi, stride, body, self.srcinfo)
end


-------------------------------------------------------------------------------
--[[                  Conversion from Lua Values into AST                  ]]--
-------------------------------------------------------------------------------

local function constant_to_ast(cval, typ, anchor)
  if typ:is_primitive() then
    if typ:is_numeric() then
      return A.NumLiteral(cval, typ, anchor.srcinfo)
    elseif typ:is_logical() then
      return A.BoolLiteral(cval, anchor.srcinfo)
    else INTERNAL_ERR('unrecognized primitive type') end
  elseif typ:is_tensor() then
    -- conversion
    local exprs           = newlist()
    local subtyp          = typ:basetype()
    local Ndim            = #typ.dims
    local dims, D         = {}, typ.dims[Ndim]
    for k=1,Ndim-1 do dims[k] = typ.dims[k] end
    if Ndim > 1 then subtyp = T.tensor(subtyp, unpack(dims)) end
    for k=1,D do
      exprs:insert( constant_to_ast(cval[k], subtyp, anchor) )
    end

    return A.ListExpr(exprs, anchor.srcinfo)
  elseif typ:is_record() then
    -- conversion
    local names, exprs    = newlist(), newlist()
    for i,p in ipairs(typ.fields) do
      names:insert( p.name )
      exprs:insert( constant_to_ast(cval[p.name], p.type, anchor) )
    end

    return A.RecordExpr(names, exprs, anchor.srcinfo)
  else
    INTERNAL_ERR('bad constant type')
  end
end

local function lua_primitive_to_ast(luav, anchor)
  if type(luav) == 'number' then
    local typ = ( is_int(luav) and T.int32 ) or T.double
    return A.NumLiteral(luav, typ, anchor.srcinfo)
  elseif type(luav) == 'boolean' then
    return A.BoolLiteral(luav, anchor.srcinfo)
  else
    INTERNAL_ERR('SHOULD only see primitive types number/boolean here')
  end
end

local function luaval_to_ast(luav, anchor)
  if is_constant(luav) then
    return constant_to_ast(luav:getvalue(), luav:type(), anchor)

  elseif is_global(luav) then
    return A.Global(luav, anchor.srcinfo)

  -- default table case
  elseif type(luav) == 'table' or type(luav) == 'string' then
    return A.LuaObj(luav, anchor.srcinfo)

  -- primitive value
  elseif type(luav) == 'number' or type(luav) == 'boolean' then
    return lua_primitive_to_ast(luav, anchor)

  -- failure case
  else
    return nil
  end
end

local function is_gong_obj(obj)
  return is_macro(obj) or is_quote(obj) or type(obj) == 'string'
                       or is_function(obj) or is_builtin(obj)
                       or is_type(obj)
end


-------------------------------------------------------------------------------
--[[                           Expression Rules                            ]]--
-------------------------------------------------------------------------------

--
--  What kinds of lua objects can we form?
--    Macro             ==>     LuaObj
--    Quote             ==>     LuaObj
--    Constant          ==>     Literals
--    tbl.name          ==>     (replace with lookup)
--    String            ==>     LuaObj
--    Function          ==>     LuaObj
--    BuiltIn           ==>     LuaObj
--

local ERROBJ = {}
function AST.Var:specialize(ctxt)
  -- First, try to look up the name in the local scope and assign
  -- the relevant symbol
  local sym = ctxt:localenv()[self.name]
  if sym then
    return A.Var(sym, self.srcinfo)
  end

  -- Second, try to look up the name in the enclosing Lua scope,
  -- converting it into an appropriate AST node
  local luaval = ctxt:luaenv()[self.name]
  if luaval ~= nil then
    -- convert lua value into an ast node
    local ast = luaval_to_ast(luaval, self)
    if ast then
      return ast
    else
      ctxt:error(self, "Could not successfully convert the Lua value "..
                       "referred to by '"..self.name.."'")
      return A.LuaObj(ERROBJ, self.srcinfo)
    end
  end

  -- Otherwise we failed to find the name
  ctxt:error(self, "variable '"..self.name.."' is undefined")
  return A.LuaObj(ERROBJ, self.srcinfo)
end

function AST.Lookup:specialize(ctxt)
  local base = self.base:specialize(ctxt)
  local args = newlist()
  for i,a in ipairs(self.args) do args[i] = a:specialize(ctxt) end

  -- if the base access is a lua object, then we might
  -- want to specialize the lookup...
  if A.LuaObj.check(base) and not is_gong_obj(base.obj) then
    local obj   = base.obj

    if type(obj) ~= 'table' then
      ctxt:error(self, "unexpected Lua object of type '"..type(obj)..
                       "'")
    elseif obj == ERROBJ then -- no-op
    elseif #args ~= 1 then
      ctxt:error(self, "expected exactly one arg")
    elseif not A.LuaObj.check(args[1]) or type(args[1].obj) ~= 'string' then
      ctxt:error(self, "expected string as argument")
    else
      local str = args[1].obj

      -- try the lookup
      local luaval = obj[str]
      if luaval == nil then
        ctxt:error(self, "Could not find entry '"..str.."' in lua table")
      else
        local ast = luaval_to_ast(luaval, self)
        if ast then return ast
        else
          ctxt:error(self, "The lua table entry '"..str.."' could not be "..
                           "successfully converted")
        end
      end
    end

    -- error fall-through
    return A.LuaObj(ERROBJ, self.srcinfo)
  end

  -- otherwise...
  return A.Lookup(base, args, self.srcinfo)
end

----------------------------------------

function AST.NumLiteral:specialize(ctxt)
  return A.NumLiteral(self.value, self.type, self.srcinfo)
end
function AST.BoolLiteral:specialize(ctxt)
  return A.BoolLiteral(self.value, self.srcinfo)
end
function AST.String:specialize(ctxt)
  return A.LuaObj(self.value, self.srcinfo)
end

-- structural
function AST.BinaryOp:specialize(ctxt)
  local lhs     = self.lhs:specialize(ctxt)
  local rhs     = self.rhs:specialize(ctxt)
  return A.BinaryOp(self.op, lhs, rhs, self.srcinfo)
end
function AST.UnaryOp:specialize(ctxt)
  local expr    = self.expr:specialize(ctxt)
  return A.UnaryOp(self.op, expr, self.srcinfo)
end
function AST.TernaryExpr:specialize(ctxt)
  local cond    = self.cond:specialize(ctxt)
  local lhs     = self.lhs:specialize(ctxt)
  local rhs     = self.rhs:specialize(ctxt)
  return A.TernaryExpr(cond, lhs, rhs, self.srcinfo)
end
function AST.LetExpr:specialize(ctxt)
  ctxt:enterblock()
  local body    = self.body:specialize(ctxt)
  local expr    = self.expr:specialize(ctxt)
  ctxt:leaveblock()
  return A.LetExpr(body, expr, self.srcinfo)
end

function AST.RecordExpr:specialize(ctxt)
  local exprs   = specialize_all(self.exprs, ctxt)
  return A.RecordExpr(self.names, exprs, self.srcinfo)
end
function AST.ListExpr:specialize(ctxt)
  local exprs   = specialize_all(self.exprs, ctxt)
  return A.ListExpr(exprs, self.srcinfo)
end

----------------------------------------

function AST.Call:specialize(ctxt)
  -- special case override
  if ctxt:in_merge_remove()   and
     AST.Var.check(self.base) and
     self.base.name == 'keep' and
     not ctxt:localenv()['keep']
  then
    local args  = specialize_all(self.args, ctxt)
    if #args ~= 1 then
      ctxt:error(self, 'expected 1 argument to keep()')
      return A.Var(NewSymbol('ERROR'),self.srcinfo)
    else
      return A.KeepExpr(args[1], self.srcinfo)
    end
  else
    local base  = self.base:specialize(ctxt)
    local args  = specialize_all(self.args, ctxt)
    return A.Call(base, args, self.srcinfo)
  end
end
function AST.TensorMap:specialize(ctxt)
  ctxt:enterblock()
  local names   = self.names:map(function(nm) return introsym(nm, ctxt) end)
  local expr    = self.expr:specialize(ctxt)
  ctxt:leaveblock()
  return A.TensorMap(names, expr, self.srcinfo)
end
function AST.TensorFold:specialize(ctxt)
  ctxt:enterblock()
  local names   = self.names:map(function(nm) return introsym(nm, ctxt) end)
  local expr    = self.expr:specialize(ctxt)
  ctxt:leaveblock()
  return A.TensorFold(self.op, names, expr, self.srcinfo)
end


