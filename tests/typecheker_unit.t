import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib
local A     = (require 'gong.src.typechecker').AST
local Util  = require 'gong.src.util'
local T     = require 'gong.src.types'

G._UNIT_TEST_TYPECHECKER = true

------------------------------------------------------------------------------

local SYM_PROTO = {}
local function SYM(nm) return setmetatable({name=nm}, SYM_PROTO) end
local function VAR(nm,typ) return { name = SYM(nm), type = typ } end

local function DOUBLE(v) return { value = v, type = G.double } end
local function FLOAT(v) return { value = v, type = G.float } end
local function INT(v) return { value = v, type = G.int32 } end
local function BOOL(v) return { value = v, type = G.bool } end

local function test_ast_match_help(ast, template, prefix)
  prefix = prefix or '<root>'
  if getmetatable(template) == SYM_PROTO then
    if not Util.is_symbol(ast) then
      error('expected a symbol at '..prefix) end
    if tostring(ast) ~= template.name then
      error("expected symbol named '"..tostring(template.name).."' "..
            "but got one named '"..tostring(ast).."'  at "..prefix) end
  elseif G.is_type(template) then
    if not G.is_type(ast) then
      error('expected a gong type at '..prefix) end
    if ast ~= template then
      error('expected the gong type "'..tostring(template)..'" but got "'..
                    tostring(ast)..'" at '..prefix) end
  elseif terralib.israwlist(template) then
    if not terralib.israwlist(ast) then
      error('expected a list at '..prefix) end
    if #template ~= #ast then
      error('expected a list of '..(#template)..' items, but got '..
                          (#ast)..' items at '..prefix) end
    for i=1,#template do
      test_ast_match_help(ast[i],template[i],prefix..'['..i..']')
    end
  elseif G.is_function(template) then
    if not G.is_function(ast) then
      error('expected a gong function at '..prefix) end
    if ast ~= template then
      error('expected to find gong function '..template:getname()..', '..
            'but found '..ast:getname()..' at '..prefix) end
  elseif type(template) == 'table' then
    if type(ast) ~= 'table' then
      error('expected a table at '..prefix) end
    for k,v in pairs(template) do
      if ast[k] == nil then
        error('expected to find entry "'..k..'" at '..prefix) end
      test_ast_match_help(ast[k], v, prefix..'.'..k)
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

local function test_ast_match( ast, template )
  if G.is_function(ast) or G.is_join(ast) then ast = ast._ast end
  return test_ast_match_help(ast, template)
end

------------------------------------------------------------------------------

gong function retzero(a : G.int32, b : G.int32) : G.int32
  return 0
end
test_ast_match( retzero,
{
  args    = { { name = SYM('a'), type = G.int32 },
              { name = SYM('b'), type = G.int32 }, },
  rettype = G.int32,
  body    = { stmts = {
    { expr = INT(0) },
  }}
})

gong function retpair()
  return 0, 1
end
test_ast_match( retpair,
{
  args    = {},
  rettype = G.record { G.int32, G.int32 },
  body    = { stmts = {
    { expr = { exprs  = { INT(0), INT(1) }, 
               type   = G.record {G.int32, G.int32} } },
  }}
})

local gong function domath(x : G.float)
  var y = x * x
  return 1.0f * y + 32f * -x + 12.0f
end
test_ast_match( domath,
{
  args = { { name = SYM('x'), type = G.float } },
  rettype = G.float,
  body = { stmts = {
    { name  = SYM('y'),
      type  = G.float,
      rval  = { op = '*', lhs = VAR('x', G.float),
                          rhs = VAR('x', G.float),
                          type = G.float } },
    { expr  = { op = '+',
                 lhs  = { op  = '+',
                          lhs = { op    = '*',
                                  lhs   = FLOAT(1),
                                  rhs   = VAR('y', G.float),
                                  type  = G.float },
                          rhs = { op    = '*',
                                  lhs   = FLOAT(32),
                                  rhs   = { op = '-', expr = VAR('x', G.float),
                                                      type = G.float },
                                  type  = G.float },
                          type = G.float,
                        },
                 rhs  = FLOAT(12),
                 type = G.float,
              } },
  }}
})

local gong function doblock()
  var x = 1
  do
    x = x + x
    return x
  end
end
test_ast_match( doblock,
{
  args    = {},
  rettype = G.int32,
  body    = { stmts = {
    { name  = SYM('x'),
      type  = G.int32,
      rval  = INT(1) },
    { lval = VAR('x', G.int32),
      rval = { op = '+', lhs = VAR('x', G.int32),
                         rhs = VAR('x', G.int32),
                         type = G.int32 } },
    { expr = VAR('x', G.int32) },
  }}
})


------------------------------------------------------------------------------

test.fail(function()
  local gong function emptyfunc() end
end, 'could not infer function return type')

local exprfunc = gong function () return 32 end
test_ast_match( exprfunc,
{
  args = {},
  rettype = G.int32,
  body = { stmts = {
    { expr = INT(32) },
  }}
})

local M = {}
gong function M.foo() return false end
test_ast_match( M.foo,
{
  args = {},
  rettype = G.bool,
  body = { stmts = {
    { expr =  BOOL(false) },
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
  args = { { name = SYM('x'), type = G.int32 } },
  rettype = G.double,
  body = { stmts = {
    { name = SYM('y'),
      type = G.double,
      rval = DOUBLE(0) },
    { lval = VAR('y', G.double),
      rval = { arg  = VAR('x', G.int32),
               type = G.double,
             } },
    { name = SYM('z'),
      type = G.double,
      rval = VAR('y', G.double) },
    { expr = VAR('z', G.double) },
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
  args = { { name = SYM('A'), type = G.mat2x3i } },
  rettype = G.vec2i,
  body = { stmts = {
    { name = SYM('x'),
      type = G.vec3i,
      rval = { exprs = { INT(1), INT(1), INT(1) }, type = G.vec3i } },
    { name = SYM('y'),
      type = G.vec2i,
      rval = { exprs = { INT(0), INT(0) }, type = G.vec2i } },
    { lval = { base = VAR('y', G.vec2i), args = { INT(0) }, type = G.int32 },
      rval = {
          op  = '+',
          lhs = { op  = '+',
                  lhs = { op  = '*',
                          lhs = { base = VAR('A', G.mat2x3i),
                                  args = {INT(0),INT(0)},
                                  type = G.int32 },
                          rhs = { base = VAR('x', G.vec3i),
                                  args = {INT(0)},
                                  type = G.int32 },
                          type = G.int32 },
                  rhs = { op  = '*',
                          lhs = { base = VAR('A', G.mat2x3i),
                                  args = {INT(0),INT(1)},
                                  type = G.int32 },
                          rhs = { base = VAR('x', G.vec3i),
                                  args = {INT(1)},
                                  type = G.int32 },
                          type = G.int32 },
                  type = G.int32 },
          rhs = { op  = '*',
                  lhs = { base = VAR('A', G.mat2x3i),
                          args = {INT(0),INT(2)},
                          type = G.int32 },
                  rhs = { base = VAR('x', G.vec3i),
                          args = {INT(2)},
                          type = G.int32 },
                  type = G.int32 },
          type = G.int32
      } },
    { lval = { base = VAR('y', G.vec2i),
               args = {INT(1)},
               type = G.int32 },
      rval = {
          op  = '+',
          lhs = { op  = '*',
                  lhs = { base = VAR('A', G.mat2x3i),
                          args = {INT(1),INT(0)},
                          type = G.int32 },
                  rhs = { base = VAR('x', G.vec3i),
                          args = {INT(0)},
                          type = G.int32 },
                  type = G.int32 },
          rhs = { op  = '+',
                  lhs = { op  = '*',
                          lhs = { base = VAR('A', G.mat2x3i),
                                  args = {INT(1),INT(1)},
                                  type = G.int32 },
                          rhs = { base = VAR('x', G.vec3i),
                                  args = {INT(1)},
                                  type = G.int32 },
                          type = G.int32 },
                  rhs = { op  = '*',
                          lhs = { base = VAR('A', G.mat2x3i),
                                  args = {INT(1),INT(2)},
                                  type = G.int32 },
                          rhs = { base = VAR('x', G.vec3i),
                                  args = {INT(2)},
                                  type = G.int32 },
                          type = G.int32 },
                  type = G.int32 },
          type = G.int32,
      } },
    { expr = VAR('y', G.vec2i) },
  }}
})

local noop = gong quote end
local gong function retone()
  noop
  return retzero(1, 2) + 1
end
test_ast_match( retone,
{
  args = {},
  rettype = G.int32,
  body = { stmts = {
    { expr = {  op  = '+',
                lhs = { func = retzero,
                        args = { INT(1), INT(2) },
                        type = G.int32 },
                rhs = INT(1),
                type = G.int32,
             } },
  }}
})


local gong function doubleassign()
  var x : G.int32 = 0
  var y : G.int32 = 0
  x,y = 1,2
  return x,y
end
local tmp = tostring(doubleassign._ast.body.stmts[3].name)
local IPAIR = G.record{G.int32,G.int32}
test_ast_match( doubleassign,
{
  args = {},
  rettype = IPAIR,
  body = { stmts = {
    { name = SYM('x'),
      type = G.int32,
      rval = INT(0) },
    { name = SYM('y'),
      type = G.int32,
      rval = INT(0) },
    { name = SYM(tmp),
      type = IPAIR,
      rval = { exprs = {INT(1),INT(2)}, type = IPAIR } },
    { lval = VAR('x',G.int32),
      rval = { base=VAR(tmp, IPAIR), arg = '_0', type = G.int32 } },
    { lval = VAR('y',G.int32),
      rval = { base=VAR(tmp, IPAIR), arg = '_1', type = G.int32 } },
    { expr = { exprs = {VAR('x',G.int32), VAR('y',G.int32)}, type = IPAIR } },
  }}
})

local recT = G.record { {'f1',double}, {'f2',double} }
local RowObjs = G.NewTable('RowObjs')
RowObjs:NewField('val', G.double)
local gong function swapfields( obj : recT, row : RowObjs )
  var temp = obj['f1']
  obj.f1 = obj.f2
  obj['f2'] = temp
  row.val   = temp
  return 0
end
test_ast_match( swapfields,
{
  args = {{ name=SYM('obj'), type=recT },
          { name=SYM('row'), type=G.row(RowObjs) }},
  rettype = G.int32,
  body = { stmts = {
    { name = SYM('temp'),
      type = G.double,
      rval = { base = VAR('obj', recT),
               arg  = 'f1',
               type = G.double  }, },
    { lval = { base = VAR('obj', recT),
               arg  = 'f1',
               type = G.double  },
      rval = { base = VAR('obj', recT),
               arg  = 'f2',
               type = G.double  } },
    { lval = { base = VAR('obj', recT),
               arg  = 'f2',
               type = G.double  },
      rval = VAR('temp', G.double) },
    { lval = { base = VAR('row', G.row(RowObjs)),
               path = {{name='val'}},
               type = G.double },
      rval = VAR('temp', G.double) },
    { expr = INT(0) },
  }}
})

local gong function tensorindexing( m : G.mat2x3i, x : G.vec4i )
  return :[i,j] +[k] m[i,k] * x[j]
end
test_ast_match( tensorindexing,
{
  args = {{name=SYM('m'), type=G.mat2x3i },
          {name=SYM('x'), type=G.vec4i }},
  rettype = G.mat2x4i,
  body = { stmts = {
    { expr = {
        names = {SYM('i'),SYM('j')},
        idxtypes = {T.tensorindex(2), T.tensorindex(4)},
        expr  = { op    = '+',
                  names = {SYM('k')},
                  idxtypes = {T.tensorindex(3)},
                  expr  = {
                      op  = '*',
                      lhs = { base = VAR('m', G.mat2x3i),
                              args = {VAR('i', T.tensorindex(2)),
                                      VAR('k', T.tensorindex(3))},
                              type = G.int32 },
                      rhs = { base = VAR('x', G.vec4i),
                              args = {VAR('j', T.tensorindex(4))},
                              type = G.int32 },
                      type = G.int32 },
                  type = G.int32 },
        type  = G.mat2x4i }
    }
  }}
})




------------------------------------------------------------------------------


local doubleplus1 = G.Macro(function(x)
  local two   = gong`x+x
  local bind  = gong quote var y = two in y+1 end
  return bind
end)
local gong function macroexprs()
  return doubleplus1(2) + doubleplus1(1+1)
end
local function PLUS(l,r,t) return { op='+', lhs=l, rhs=r, type=t } end
test_ast_match( macroexprs,
{
  args = {},
  rettype = G.int32,
  body = { stmts = {
    { expr = {  op  = '+',
        lhs = { block = { stmts = {
                    { name = SYM('y'),
                      type = G.int32,
                      rval = PLUS( INT(2), INT(2), G.int32 ) }
                  } },
                expr  = PLUS( VAR('y',G.int32), INT(1), G.int32 ),
                type  = G.int32 },
        rhs = { block = { stmts = {
                    { name = SYM('y'),
                      type = G.int32,
                      rval = PLUS( PLUS(INT(1), INT(1), G.int32),
                                   PLUS(INT(1), INT(1), G.int32),
                                   G.int32 ) }
                  } },
                expr  = PLUS( VAR('y',G.int32), INT(1), G.int32 ),
                type  = G.int32 },
        type = G.int32,
    } },
  }}
})

local A = G.NewTable('A')
local B = G.NewTable('B')
local C = G.NewTable('C')
A:NewField('id', G.int32)
B:NewField('id', G.int32)
C:NewField('a', A)
C:NewField('b', B)
local gong join aboff( a : A, b : B )
  where a.id+1 == b.id
do
  emit { a=a, b=b } in C
end
test_ast_match( aboff,
{
  args = { { name = SYM('a'), type = G.row(A) },
           { name = SYM('b'), type = G.row(B) } },
  filter = { stmts = {
    { expr = { op   = '==',
               lhs  = { op    = '+',
                        lhs   = { base = VAR('a',G.row(A)),
                                  path = {{name='id'}},
                                  type = G.int32 },
                        rhs   = INT(1),
                        type  = G.int32 },
               rhs  = { base = VAR('b',G.row(B)),
                        path = {{name='id'}},
                        type = G.int32 },
               type = G.bool,
    }}
  }},
  doblock = { stmts = {
    { record  = { exprs = {
                  VAR('a', G.row(A)),
                  VAR('b', G.row(B)),
                }, type = G.record{ {'a',G.row(A)}, {'b',G.row(B)} } },
      dst     = G.row(C) }
  }}
})

gong function retpair_annotate() : { G.int32, G.int32 }
  return 0, 1
end
test_ast_match( retpair_annotate,
{
  args    = {},
  rettype = G.record { G.int32, G.int32 },
  body    = { stmts = {
    { expr = { exprs  = { INT(0), INT(1) }, 
               type   = G.record {G.int32, G.int32} } },
  }}
})

gong function if_some( a : G.int32 )
  var r : G.int32 = 0
  if      a > 0 then r = 32
  elseif a == 0 then r = 64
                else r = 128 end
  return r
end
test_ast_match( if_some,
{
  args    = {{ name = SYM('a'), type = G.int32 }},
  rettype = G.int32,
  body    = { stmts = {
    { name = SYM('r'),    type = G.int32,   rval = INT(0) },
    { cases = {
        { cond = { op='>', lhs=VAR('a',G.int32), rhs=INT(0), type = G.bool },
          body = { stmts = {
            { lval = VAR('r',G.int32), rval = INT(32) }
          }} },
        { cond = { op='==', lhs=VAR('a',G.int32), rhs=INT(0), type = G.bool },
          body = { stmts = {
            { lval = VAR('r',G.int32), rval = INT(64) }
          }} },
      },
      else_body = { stmts = {
        { lval = VAR('r',G.int32), rval = INT(128) }
      }} },
    { expr = VAR('r',G.int32) },
  }}
})

gong function for_loop( a : G.int32 )
  for k=0,20,2 do a = a+k end
  return a
end
test_ast_match( for_loop,
{
  args    = {{ name = SYM('a'), type = G.int32 }},
  rettype = G.int32,
  body    = { stmts = {
    { itername  = SYM('k'),
      lo = INT(0),   hi = INT(20),  stride = INT(2),
      body = { stmts = {
        { lval = VAR('a', G.int32),
          rval = { op='+', lhs=VAR('a',G.int32),
                           rhs=VAR('k',G.int32), type=G.int32 } },
      }}
    },
    { expr = VAR('a',G.int32) },
  }}
})

gong function assignpair(a : G.int32, b : G.int32)
  a,b = retpair()
  return a + b
end
local tmp = tostring(assignpair._ast.body.stmts[1].name)
test_ast_match( assignpair,
{
  args    = { {name=SYM('a'), type=G.int32}, {name=SYM('b'), type=G.int32} },
  rettype = G.int32,
  body    = { stmts = {
    { name = SYM(tmp), type = IPAIR,
      rval = { func = retpair, args = {}, type = IPAIR } },
    { lval = VAR('a',G.int32),
      rval = { base = VAR(tmp,IPAIR), arg = '_0', type = G.int32 } },
    { lval = VAR('b',G.int32),
      rval = { base = VAR(tmp,IPAIR), arg = '_1', type = G.int32 } },
    { expr = { op   = '+',
               lhs  = VAR('a',G.int32),
               rhs  = VAR('b',G.int32),
               type = G.int32 } },
  }}
})

gong function declpairs()
  var a,b = retpair()
  var x,y = b,a
  return x + y
end
local tmp = tostring(declpairs._ast.body.stmts[1].name)
local tmp2 = tostring(declpairs._ast.body.stmts[4].name)
test_ast_match( declpairs,
{
  args    = {},
  rettype = G.int32,
  body    = { stmts = {
    { name = SYM(tmp), type = IPAIR,
      rval = { func = retpair, args = {}, type = IPAIR } },
    { name = SYM('a'), type = G.int32,
      rval = { base = VAR(tmp,IPAIR), arg = '_0', type = G.int32 } },
    { name = SYM('b'), type = G.int32,
      rval = { base = VAR(tmp,IPAIR), arg = '_1', type = G.int32 } },
    { name = SYM(tmp2), type = IPAIR,
      rval = { exprs = { VAR('b',G.int32), VAR('a',G.int32) },
               type  = IPAIR } },
    { name = SYM('x'), type = G.int32,
      rval = { base = VAR(tmp2,IPAIR), arg = '_0', type = G.int32 } },
    { name = SYM('y'), type = G.int32,
      rval = { base = VAR(tmp2,IPAIR), arg = '_1', type = G.int32 } },
    { expr = { op   = '+',
               lhs  = VAR('x',G.int32),
               rhs  = VAR('y',G.int32),
               type = G.int32 } },
  }}
})

gong function redstmt( a : G.int32 )
  a += 20
  return a
end
test_ast_match( redstmt,
{
  args    = {{ name = SYM('a'), type = G.int32 }},
  rettype = G.int32,
  body    = { stmts = {
    { op    = '+',
      lval  = VAR('a',G.int32),
      rval  = INT(20), },
    { expr = VAR('a',G.int32) },
  }}
})


------------------------------------------------------------------------------









