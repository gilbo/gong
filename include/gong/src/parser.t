import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.parser"] = Exports


local Util        = require 'gong.src.util'
local SrcInfo     = Util.SrcInfo
local is_id_str   = Util.is_id_str
local is_int      = Util.is_int
local T           = require 'gong.src.types'
local pratt       = require 'gong.src.pratt'

local newlist     = terralib.newlist

-------------------------------------------------------------------------------

local lang        = {}

function Exports.ParseExpression(lexer)
  return pratt.Parse(lang, lexer, 'expression_entry')
end

function Exports.ParseStatement(lexer)
  return pratt.Parse(lang, lexer, 'statement_entry')
end

-------------------------------------------------------------------------------
--                                    AST                                    --
-------------------------------------------------------------------------------

local binop_token_set = {
  ['+']     = true,
  ['-']     = true,
  ['*']     = true,
  ['/']     = true,
  ['%']     = true,
  ['<']     = true,
  ['>']     = true,
  ['<=']    = true,
  ['>=']    = true,
  ['==']    = true,
  ['~=']    = true,
  ['and']   = true,
  ['or']    = true,
}
local unop_token_set = {
  ['-']     = true,
  ['not']   = true,
}
local redop_token_set = {
  ['+']    = true,
  ['*']    = true,
  ['min']  = true,
  ['max']  = true,
}

Exports.binop_token_set     = binop_token_set
Exports.unop_token_set      = unop_token_set
Exports.redop_token_set     = redop_token_set

local function is_func(arg) return type(arg) == 'function' end

-- Definition of AST structure produced by the parser
-- This just slightly abstracts the concrete syntax,
-- and will be quickly converted to more meaningful
-- ASTs/IRs at definition time.
local ADT A
  -- Top Level Forms
  Quote       = { body    : Block?,
                  expr    : Expr?,            srcinfo : SrcInfo }
  Function    = { name    : string,
                  args    : ArgDecl*,
                  rettype : LuaType?,
                  body    : Block,            srcinfo : SrcInfo }
  Join        = { name    : string,
                  args    : ArgDecl*,
                  filter  : Block,            
                  doblock : Block,            srcinfo : SrcInfo }

  -- Shared Structural Components
  ArgDecl     = { name    : id_str,
                  type    : LuaType,          srcinfo : SrcInfo }
  IfCase      = { cond    : Expr,
                  body    : Block,            srcinfo : SrcInfo }
  Block       = { stmts   : Stmt*,            srcinfo : SrcInfo }

  -- temporary throw-away
  RedOp       = { op : redop,   lhs : Expr,   srcinfo : SrcInfo }

  -- Statements
  Stmt  =
    -- Terminal / Effect Statements
          WhereFilter { expr      : Expr }
        | EmitStmt    { record    : RecordExpr,
                        dst_table : LuaTable }
        | ExprStmt    { expr      : Expr }
        | ReturnStmt  { exprs     : Expr* }
    -- Basic Structural Statements
        | IfStmt      { cases     : IfCase*,  else_body : Block? }
        | DoStmt      { body      : Block }
    -- Binding & Assignment
        | Assignment  { lvals     : Expr*,    rvals     : Expr* }
        | Reduction   { op        : redop,
                        lval      : Expr,     rval      : Expr }
        | DeclStmt    { names     : id_str*,  type      : LuaType?,
                        rvals     : Expr* }
    -- Looping
        | ForLoop     { itername  : id_str,
                        lo        : Expr,     hi        : Expr,
                        stride    : Expr?,
                        body      : Block }
        attributes {                          srcinfo : SrcInfo }

  -- Expressions
  Expr =
    -- Literals and other Atoms
          Var         { name  : id_str }
        | NumLiteral  { value : number,       type      : GongType }
        | BoolLiteral { value : boolean }
        | String      { value : string }
    -- Basic building up of conditions and terms
        | BinaryOp    { op    : binop,
                        lhs   : Expr,         rhs       : Expr }
        | UnaryOp     { op    : unop,         expr      : Expr }
        | TernaryExpr { cond  : Expr,
                        lhs   : Expr,         rhs       : Expr }
        | LetExpr     { block : Block,        expr      : Expr }
    -- Data Constructors
        | RecordExpr  { names : id_str*,      exprs     : Expr* }
        | ListExpr    { exprs : Expr* }
    -- Data Destructors
        -- lookup is base[a1,...]
        | Lookup      { base  : Expr,         args      : Expr* }
    -- Special forms
        -- call is base(a1,...)
        | Call        { base  : Expr,         args      : Expr* }
        | TensorMap   { names : id_str*,      expr      : Expr  }
        | TensorFold  { op    : redop,
                        names : id_str*,      expr      : Expr  }
        attributes {                          srcinfo : SrcInfo }

  extern binop  function(obj)
    return type(obj) == 'string' and binop_token_set[obj]
  end
  extern unop   function(obj)
    return type(obj) == 'string' and unop_token_set[obj]
  end
  extern redop  function(obj)
    return type(obj) == 'string' and redop_token_set[obj]
  end

  extern LuaType  is_func
  extern LuaTable is_func

  extern GongType function(obj) return T.is_type(obj) end

  extern id_str   is_id_str
  extern SrcInfo  function(obj) return SrcInfo.check(obj) end
end
Exports.AST = A


-------------------------------------------------------------------------------
--                                  Parsing                                  --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Basics

function lang.srcinfo(P)
  return SrcInfo(P:cur().linenumber,
                 P:cur().offset,
                 P.source)
end

function lang.id_str(P, id)
  if not id then id = P:expect(P.name).value end
  if not is_id_str(id) then
    P:error('expected valid identifier.  May only use _ latin letters and '..
            'digits.  Idetifier must start with a non-digit character')
  end
  return id
end

function lang.expectname(P, nm)
  local id = P:expect(P.name).value
  if id ~= nm then
    P:error('expected '..nm)
  end
end

function lang.gen_anon_name(P)
  local anonname = "anon_"..tostring(P.source)..
                       "_"..tostring(P:cur().linenumber)
  anonname = string.gsub(anonname, "[^%a%d_]","_")
  return anonname
end

function lang.func_name(P)
  local namestr   = P:id_str()
  local nametuple = newlist { namestr }
  while P:nextif('.') do
    local nextname = P:id_str()
    nametuple:insert(nextname)
    namestr = namestr..'.'..nextname
  end
  return namestr, { nametuple }
end



-------------------------------------------------------------------------------
-- Expressions

-- Precedence Table
--  00
--    :[]
--    +[]
--    *[]
--    :min[] :max[]
--    let
--  08
--    ? :
--  10
--    or and
--  20
--    == ~=
--  30
--    < > <= >=
--  40
--    + -
--  50
--    * / %
--  60
--    unary not, -
--  70
--  80
--    [   sq access
--    (   function calls
--    .   data access
--  90

local function parse_binop(P, lhs, is_redop)
  local info  = P:srcinfo()
  local op    = P:next().type
  if is_redop and P:matches('=') then
    return A.RedOp(op, lhs, info)
  else
    return A.BinaryOp(op, lhs, P:expr(op), info)
  end
end

local function parse_binop_red(P, lhs)
  return parse_binop(P,lhs,true)
end

local function parse_unop(P)
  local info  = P:srcinfo()
  local op    = P:next().type
  return A.UnaryOp(op, P:expr(60), info)
end

local function parse_tensorfold(P)
  local info      = P:srcinfo()
  local op        = P:next().type
  local parens    = P:srcinfo()
  -- handle special :min and :max operators
  if op == ':' and P:matches(P.name) then
    local tkn = P:cur()
    if tkn.value == 'max' then
                    P:next();       op = 'max'
    elseif tkn.value == 'min' then
                    P:next();       op = 'min'
    end
  end
  -- handle index variables
                    P:expect('[')
  local names     = newlist()
  repeat
    names:insert(   P:id_str() )
  until not         P:nextif(',')
                    P:expectmatch(']','[', info.linenumber)
  if op == ':' then
    return A.TensorMap(names, P:expr(), info)
  else
    return A.TensorFold(op, names, P:expr(), info)
  end
end


lang.expr = pratt.Pratt()
:prefix('-',      parse_unop)
:prefix('not',    parse_unop)
:prefix('+',      parse_tensorfold)
:prefix('*',      parse_tensorfold)
:prefix(':',      parse_tensorfold)
:prefix('let',    function(P)
  local info      = P:srcinfo()
                    P:expect('let')
  local block     = P:block()
                    P:expectmatch('in', 'let', info.linenumber)
  local expr      = P:expr()
                    P:expectmatch('end', 'let', info.linenumber)
  return A.LetExpr(block, expr, info)
end)
:infix('?',       function(P, cond)
  local info      = P:srcinfo()
                    P:expect('?')
  local lhs       = P:expr(8, 'right')
                    P:expectmatch('else', '?', info.linenumber)
  local rhs       = P:expr(8, 'right')
  return A.TernaryExpr(cond, lhs, rhs, info)
end)
:infix('or',  10, parse_binop)
:infix('and', 10, parse_binop)
:infix('==',  20, parse_binop)
:infix('~=',  20, parse_binop)
:infix('<',   30, parse_binop)
:infix('>',   30, parse_binop)
:infix('<=',  30, parse_binop)
:infix('>=',  30, parse_binop)
:infix('+',   40, parse_binop_red)
:infix('-',   40, parse_binop)
:infix('*',   50, parse_binop_red)
:infix('/',   50, parse_binop)
:infix('%',   50, parse_binop)
:infix('[',   80, function(P, base)
  local info      = P:srcinfo()
                    P:expect('[')
  local args      = newlist()
  repeat
    args:insert(    P:expr() )
  until         not P:nextif(',')
                    P:expectmatch(']','[',info.linenumber)
  return A.Lookup(base, args, info)
end)
:infix('.',   80, function(P, base)
  local info      = P:srcinfo()
                    P:expect('.')
  local nminfo    = P:srcinfo()
  local name      = P:id_str()
  return A.Lookup(base, newlist{ A.String(name, nminfo) }, info)
end)
:infix('(',   80, function(P, base)
  local info      = P:srcinfo()
                    P:expect('(')
  local args      = newlist()
  if not P:nextif(')') then
    repeat
      args:insert(  P:expr() )
    until       not P:nextif(',')
                    P:expectmatch(')','(',info.linenumber)
  end
  return A.Call(base, args, info)
end)
:prefix(pratt.default, function(P)
  local info      = P:srcinfo()

  if      P:matches(P.name) then
    local name    = P:id_str();   P:ref(name) -- possible up-value
    return A.Var(name, info)

  elseif  P:nextif('true') then   return A.BoolLiteral(true, info)
  elseif  P:nextif('false') then  return A.BoolLiteral(false, info)

  elseif  P:matches(P.string) then
    return A.String( P:next().value, info)

  elseif  P:matches(P.number) then
    local tkn     = P:next()
    local valtyp  = T.fromterratype(tkn.valuetype)
    if valtyp == nil then
      P:error('unrecognized numeric literal type: '..tostring(tkn.valuetype))
    end
    return A.NumLiteral(tkn.value, valtyp, info)

  elseif  P:nextif('(') then
    local expr    = P:expr()
                    P:expectmatch(')','(',info.linenumber)
    return expr

  elseif  P:nextif('{') then
    -- detect possible record
    if P:cur().type == P.name and P:lookahead().type == '=' then
      return P:record_expr(info)
    else -- list
      local exprs   = newlist()
      repeat
        exprs:insert( P:expr() )
      until       not P:nextif(',')
                      P:expectmatch('}','{',info.linenumber)
      return A.ListExpr(exprs, info)
    end
  else
    P:error('unexpected symbol when trying to parse expression')
  end
end)


function lang.record_expr(P, info)
  local names, exprs  = newlist(), newlist()
  repeat
    names:insert(   P:id_str() )
                    P:expect('=')
    exprs:insert(   P:expr() )
  until         not P:nextif(',')
                    P:expectmatch('}','{',info.linenumber)
  return A.RecordExpr(names, exprs, info)
end

-------------------------------------------------------------------------------
-- Statements

local block_terminators = {
  ['end']     = true,
  ['in']      = true,
  ['elseif']  = true,
  ['else']    = true,
}

function lang.block(P, is_join_filter)
  local info      = P:srcinfo()
  local stmts     = newlist()
  while not block_terminators[P:cur().type] do
    local stmt    = P:stmt(is_join_filter)
    if stmt == nil  then break
                    else stmts:insert(stmt) end
                    P:nextif(';')
  end
  return A.Block(stmts, info)
end

-- can return nil to cut off block parsing
function lang.stmt(P, is_join_filter)
  local info        = P:srcinfo()

  if      P:matches('do') then
    if is_join_filter then return nil end
                      P:expect('do')
    local block     = P:block()
                      P:expectmatch('end','do',info.linenumber)
    return A.DoStmt(block, info)

  elseif  P:nextif('where') then
    local expr      = P:expr()
    return A.WhereFilter(expr, info)

  elseif  P:nextif('return') then
    local exprs     = newlist()
    repeat
      exprs:insert(   P:expr() )
    until         not P:nextif(',')
    return A.ReturnStmt(exprs, info)

  elseif  P:nextif('emit') then
    local openinfo  = P:srcinfo()
                      P:expect('{')
    local recexpr   = P:record_expr(openinfo)
                      P:expect('in')
    local dst_table = P:luaexpr()
    return A.EmitStmt(recexpr, dst_table, info)

  elseif  P:nextif('if') then
    -- parse the first if and any number of elseif-cases
    local cases     = newlist()
    local lastinfo  = info
    local lasttkn   = 'if'
    repeat
      local cond    = P:expr()
                      P:expectmatch('then',lasttkn,lastinfo.linenumber)
      local body    = P:block()
      cases:insert(   A.IfCase(cond, body, lastinfo) )
      lastinfo      = P:srcinfo()
      lasttkn       = 'elseif'
    until         not P:nextif('elseif')

    -- optional else block
    local else_body = nil
    if P:nextif('else') then
      else_body     = P:block()
    end
                      P:expectmatch('end','if',info.linenumber)
    return A.IfStmt(cases, else_body, info)

  elseif  P:nextif('for') then
    local itername  = P:id_str()
                      P:expect('=')
    local lo        = P:expr()
                      P:expect(',')
    local hi        = P:expr()
    local stride    = nil
    if P:nextif(',') then
      stride        = P:expr()
    end
                      P:expectmatch('do','for',info.linenumber)
    local body      = P:block()
                      P:expectmatch('end','for',info.linenumber)
    return A.ForLoop(itername, lo, hi, stride, body, info)

  elseif  P:nextif('var') then
    -- part 1: names
    local names     = newlist()
    repeat
      names:insert(   P:id_str() )
    until         not P:nextif(',')

    -- part 2: maybe a type
    local typ       = nil
    if #names == 1 and P:nextif(':') then
      typ           = P:luaexpr()
    end

    -- part 3: rhs
                      P:expect('=')
    local rvals     = newlist()
    repeat
      rvals:insert(   P:expr() )
    until         not P:nextif(',')
    -- final
    return A.DeclStmt(names, typ, rvals, info)

  else
    -- ExprStmt, Assignment, Reduction
    local exprs     = newlist()
    repeat
      exprs:insert(   P:expr() )
    until         not P:nextif(',')

    -- reduction unpack
    local red_op    = nil
    if #exprs == 1 and A.RedOp.check(exprs[1]) then
      red_op        = exprs[1].op
      exprs[1]      = exprs[1].lhs
                      P:expect('=')
    elseif redop_token_set[ P:cur().type ] or 
      ( P:matches(P.name) and ( P:cur().value == 'min' or
                                P:cur().value == 'max' ) )
    then
      if #exprs > 1 then
        P:error('unexpected comma separated expressions') end
      red_op        = P:cur().type
      if P:matches(P.name) then
        red_op      = P:cur().value
      end
                      P:next()
      info          = P:srcinfo()
                      P:expect('=')
    end

    -- REDUCTION
    if red_op then
      local rval    = P:expr()
      return A.Reduction(red_op, exprs[1], rval, info)

    -- ASSIGNMENT
    elseif P:matches('=') then
      info          = P:srcinfo()
                      P:expect('=')
      local rvals   = newlist()
      repeat
        rvals:insert( P:expr() )
      until       not P:nextif(',')
      return A.Assignment(exprs, rvals, info)

    -- EXPR STMT
    else
      if #exprs > 1 then P:error('unexpected comma separated expressions') end
      return A.ExprStmt(exprs[1], info)

    end
  end
end

-------------------------------------------------------------------------------
-- Top Level Forms

function lang.quote_expr(P)
  local info        = P:srcinfo()
                      P:expect('`')
  local expr        = P:expr()
  return A.Quote(nil, expr, info)
end

function lang.quot(P)
  local info        = P:srcinfo()
                      P:expect('quote')
  local body        = P:block()
  local expr       = (P:nextif('in') and
                        P:expr()) or nil
                      P:expectmatch('end','quote',info.linenumber)
  return A.Quote(body, expr, info)
end

function lang.func(P)
  local info        = P:srcinfo()
                      P:expect('function')
  local name, tuple = nil, nil
  if P:matches('(')
  then name         = P:gen_anon_name()
  else name, tuple  = P:func_name()       end
  local openparen   = P:srcinfo()
                      P:expect('(')
  local args        = newlist()
  if not P:nextif(')') then
    repeat
      args:insert(    P:argdecl() )
    until         not P:nextif(',')
                      P:expectmatch(')','(',openparen.linenumber)
  end
  local rettype   = ( P:nextif(':')
                    and P:luaexpr() ) or nil
  local body        = P:block()
                      P:expectmatch('end','function', info.linenumber)
  return A.Function(name, args, rettype, body, info), tuple
end

function lang.join(P)
  local info        = P:srcinfo()
                      P:expectname('join')
  local name, tuple = nil, nil
  if P:matches('(')
  then name         = P:gen_anon_name()
  else name, tuple  = P:func_name()       end
  local openparen   = P:srcinfo()
                      P:expect('(')
  local args        = newlist()
  if not P:nextif(')') then
    repeat
      args:insert(    P:argdecl() )
    until         not P:nextif(',')
                      P:expectmatch(')','(',openparen.linenumber)
  end
  local filter      = P:block('inside join filter')
                      P:expectmatch('do','join',info.linenumber)
  local dobody      = P:block()
                      P:expectmatch('end','join', info.linenumber)
  return A.Join(name, args, filter, dobody, info), tuple
end

function lang.argdecl(P)
  local info        = P:srcinfo()
  local name        = P:id_str()
                      P:expect(':')
  local typ         = P:luaexpr()
  return A.ArgDecl(name, typ, info)
end

-------------------------------------------------------------------------------
-- Top Level Forms

function lang.statement_entry(P)
           P:expect('gong')
  if P:matches('function') then
    return P:func()
  elseif P:matches(P.name) and P:cur().value == 'join' then
    return P:join()
  else
    P:error("expected 'function' or 'join' following 'gong'")
  end
end

function lang.expression_entry(P)
           P:expect('gong')
  if P:matches('`') then
    return P:quote_expr()
  elseif P:matches('quote') then
    return P:quot()
  elseif P:matches('function') then
    return P:func()
  elseif P:matches(P.name) and P:cur().value == 'join' then
    return P:join()
  else
    P:error("expected '`', 'quote', 'function' or 'join' following 'gong'")
  end
end








