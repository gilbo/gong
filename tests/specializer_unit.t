import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib
local A     = (require 'gong.src.specializer').AST
local Util  = require 'gong.src.util'
--local T     = require 'gong.src.types'

-- flip a flag to enable unit testing
G._UNIT_TEST_SPECIALIZER = true

------------------------------------------------------------------------------

local SYM_PROTO = {}
local function SYM(nm) return setmetatable({name=nm}, SYM_PROTO) end
local function VAR(nm) return { name = SYM(nm) } end

local function DOUBLE(v) return { value = v, type = G.double } end
local function FLOAT(v) return { value = v, type = G.float } end
local function INT(v) return { value = v, type = G.int32 } end
local function BOOL(v) return { value = v } end

local function test_ast_match(ast, template, prefix)
  prefix = prefix or '<root>'
  if getmetatable(template) == SYM_PROTO then
    if not Util.is_symbol(ast) then
      error('expected a symbol at '..prefix) end
    if tostring(ast) ~= template.name then
      error("expected symbol named '"..template.name.."' but got one "..
            "named '"..tostring(ast).."'  at "..prefix) end
  elseif G.is_type(template) then
    if not G.is_type(ast) then
      error('expected a gong type at '..prefix) end
    if ast ~= template then
      error('expected the gong type "'..template..'" but got "'..
                    tostring(ast)..'" at '..prefix) end
  elseif terralib.israwlist(template) then
    if not terralib.israwlist(ast) then
      error('expected a list at '..prefix) end
    if #template ~= #ast then
      error('expected a list of '..(#template)..' items, but got '..
                          (#ast)..' items at '..prefix) end
    for i=1,#template do
      test_ast_match(ast[i],template[i],prefix..'['..i..']')
    end
  elseif type(template) == 'table' then
    if type(ast) ~= 'table' then
      error('expected a table at '..prefix) end
    for k,v in pairs(template) do
      if ast[k] == nil then
        error('expected to find entry "'..k..'" at '..prefix) end
      test_ast_match(ast[k], v, prefix..'.'..k)
    end
    for k,_ in pairs(ast) do
      if k ~= 'srcinfo' and template[k] == nil then
        error('found unexpected table entry "'..k..'" at '..prefix) end
    end
  else
    if template ~= ast then
      error('expected value  '..tostring(template)..'  but got  '..
                                tostring(ast)..'  at '..prefix) end
  end
end

------------------------------------------------------------------------------

gong function retzero(a : G.int32, b : G.int32)
  return 0
end
test_ast_match( retzero,
{
  name = 'retzero',
  args = { { name = SYM('a'), type = G.int32 },
           { name = SYM('b'), type = G.int32 }, },
  body = { stmts = {
    { exprs = { INT(0) } },
  }}
})

gong function retpair()
  return 0, 1
end
test_ast_match( retpair,
{
  name = 'retpair',
  args = {},
  body = { stmts = {
    { exprs = { INT(0), INT(1) } },
  }}
})

local gong function domath(x : G.float)
  var y = x * x
  return 1.0f * y + 32f * -x + 12.0f
end
test_ast_match( domath,
{
  name = 'domath',
  args = { { name = SYM('x'), type = G.float } },
  body = { stmts = {
    { names = {SYM('y')},
      type  = nil,
      rvals = {{ op = '*', lhs = VAR('x'), rhs = VAR('x') }} },
    { exprs = {{ op = '+',
                 lhs = {  op = '+',
                          lhs = { op  = '*',
                                  lhs = FLOAT(1),
                                  rhs = VAR('y') },
                          rhs = { op  = '*',
                                  lhs = FLOAT(32),
                                  rhs = { op = '-', expr = VAR('x') } }
                       },
                 rhs = FLOAT(12),
              }} },
  }}
})

local gong function doblock()
  var x = 1
  do
    x = x + x
  end
end
test_ast_match( doblock,
{
  name = 'doblock',
  args = {},
  body = { stmts = {
    { names = {SYM('x')},
      type  = nil,
      rvals = { INT(1) } },
    { body = { stmts = {
        { lvals = { VAR('x') },
          rvals = {{ op = '+', lhs = VAR('x'), rhs = VAR('x') }} },
      }} },
  }}
})


------------------------------------------------------------------------------


local gong function emptyfunc() end
test_ast_match( emptyfunc,
{
  name = 'emptyfunc',
  args = {},
  body = { stmts = {} }
})

local exprfunc = gong function () return 32 end
test_ast_match( exprfunc,
{
  name = exprfunc.name,   -- cheat.  We don't know anonymous name
  args = {},
  body = { stmts = {
    { exprs = { INT(32) } },
  }}
})

local M = {}
gong function M.foo() return false end
test_ast_match( M.foo,
{
  name = 'M.foo',
  args = {},
  body = { stmts = {
    { exprs = { BOOL(false) } },
  }}
})


------------------------------------------------------------------------------


local gong function annotate( x : G.int32 )
  var y : G.double = 0.0
  y = G.double(x)
  var z : G.double = y
  return z
end
test_ast_match( annotate,
{
  name = 'annotate',
  args = { { name = SYM('x'), type = G.int32 } },
  body = { stmts = {
    { names = { SYM('y') },
      type  = G.double,
      rvals = { DOUBLE(0) } },
    { lvals = { VAR('y') },
      rvals = {{  base = { obj = G.double },
                  args = { VAR('x') },
              }} },
    { names = { SYM('z') },
      type  = G.double,
      rvals = { VAR('y') } },
    { exprs = { VAR('z') } },
  }}
})

gong function vecmat( A : G.mat2x3i )
  var x = { 1, 1, 1 }
  var y = { 0, 0 }
  y[0] = A[0,0] * x[0] + A[0,1] * x[1] + A[0,2] * x[2]
  y[1] = A[1,0] * x[0] + (A[1,1] * x[1] + A[1,2] * x[2])
  return y
end
test_ast_match( vecmat,
{
  name = 'vecmat',
  args = { { name = SYM('A'), type = G.mat2x3i } },
  body = { stmts = {
    { names = { SYM('x') },
      type  = nil,
      rvals = { {exprs = { INT(1), INT(1), INT(1) }} } },
    { names = { SYM('y') },
      type  = nil,
      rvals = { {exprs = { INT(0), INT(0) }} } },
    { lvals = {{ base = VAR('y'), args = {INT(0)} }},
      rvals = {{
          op  = '+',
          lhs = { op  = '+',
                  lhs = { op  = '*',
                          lhs = { base = VAR('A'), args = {INT(0),INT(0)} },
                          rhs = { base = VAR('x'), args = {INT(0)} } },
                  rhs = { op  = '*',
                          lhs = { base = VAR('A'), args = {INT(0),INT(1)} },
                          rhs = { base = VAR('x'), args = {INT(1)} } } },
          rhs = { op  = '*',
                  lhs = { base = VAR('A'), args = {INT(0),INT(2)} },
                  rhs = { base = VAR('x'), args = {INT(2)} } },
      }} },
    { lvals = {{ base = VAR('y'), args = {INT(1)} }},
      rvals = {{
          op  = '+',
          lhs = { op  = '*',
                  lhs = { base = VAR('A'), args = {INT(1),INT(0)} },
                  rhs = { base = VAR('x'), args = {INT(0)} } },
          rhs = { op  = '+',
                  lhs = { op  = '*',
                          lhs = { base = VAR('A'), args = {INT(1),INT(1)} },
                          rhs = { base = VAR('x'), args = {INT(1)} } },
                  rhs = { op  = '*',
                          lhs = { base = VAR('A'), args = {INT(1),INT(2)} },
                          rhs = { base = VAR('x'), args = {INT(2)} } } },
      }} },
    { exprs = { VAR('y') } },
  }}
})

local noop = {}
local gong function retone()
  noop
  return retzero() + 1
end
test_ast_match( retone,
{
  name = 'retone',
  args = {},
  body = { stmts = {
    { expr = {obj = noop} },
    { exprs = {{  op  = '+',
                  lhs = { base = {obj = retzero},
                          args = {} },
                  rhs = INT(1),
              }} },
  }}
})


local gong function doubleassign()
  var x : G.int32 = 0
  var y : G.int32 = 0
  x,y = 1,2
  return x,y
end
test_ast_match( doubleassign,
{
  name = 'doubleassign',
  args = {},
  body = { stmts = {
    { names = {SYM('x')},
      type  = G.int32,
      rvals = { INT(0) } },
    { names = {SYM('y')},
      type  = G.int32,
      rvals = { INT(0) } },
    { lvals = {VAR('x'), VAR('y')},
      rvals = { INT(1), INT(2) } },
    { exprs = {VAR('x'), VAR('y')} },
  }}
})

local recT = G.record { {'f1',double}, {'f2',double} }
local gong function swapfields( obj : recT )
  var temp = obj['f1']
  obj.f1 = obj.f2
  obj['f2'] = temp
end
test_ast_match( swapfields,
{
  name = 'swapfields',
  args = {{ name=SYM('obj'), type=recT }},
  body = { stmts = {
    { names = {SYM('temp')},
      type  = nil,
      rvals = {{ base = VAR('obj'), args={{obj='f1'}} }}, },
    { lvals = {{ base = VAR('obj'), args={{obj='f1'}} }},
      rvals = {{ base = VAR('obj'), args={{obj='f2'}} }} },
    { lvals = {{ base = VAR('obj'), args={{obj='f2'}} }},
      rvals = {VAR('temp')} },
  }}
})

local gong function tensorindexing( m : G.mat2x3i, x : G.vec4i )
  return :[i,j] +[k] m[i,k] * x[j]
end
test_ast_match( tensorindexing,
{
  name = 'tensorindexing',
  args = {{name=SYM('m'), type=G.mat2x3i },
          {name=SYM('x'), type=G.vec4i }},
  body = { stmts = {
    { exprs = {{
        names = {SYM('i'),SYM('j')},
        expr  = { op    = '+',
                  names = {SYM('k')},
                  expr  = {
                      op  = '*',
                      lhs = { base = VAR('m'),
                              args = {VAR('i'),VAR('k')} },
                      rhs = { base = VAR('x'),
                              args = {VAR('j')} }
                  } }
              }} }
  }}
})


