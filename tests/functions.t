import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib

local GPU_ON = not not terralib.cudacompile

------------------------------------------------------------------------------


local A   = G.NewTable('A')
local OUT = G.NewTable('OUT')
A:NewField('id', G.int32)
OUT:NewField('lhs', A)
OUT:NewField('rhs', A)
local gong function add_one( i : G.int32 )
  return i + 1
end
local gong function off_left( i : G.int32, j : G.int32 )
  return add_one(i) == j
end
local gong function off_right( i : G.int32, j : G.int32 )
  return i == add_one(j)
end
local gong function off_by_one( i : G.int32, j : G.int32 )
  return off_left(i,j) or off_right(i,j)
end
local gong join self_join( lhs : A, rhs : A )
  where off_by_one(lhs.id, rhs.id)
do
  emit { lhs=lhs, rhs=rhs } in OUT
end

if GPU_ON then
  OUT:setGPUSizeLinear(1, A, 10)
end


------------------------------------------------------------------------------

local API = G.CompileLibrary {
  tables        = {},
  joins         = {self_join},
  terra_out     = true,
  gpu           = GPU_ON,
}

local C   = terralib.includecstring [[
#include<stdio.h>
]]
local CHECK_ERR = macro(function(store)
  return quote if store:geterror() ~= nil then
    C.printf("%s\n", store:geterror())
    store:destroy()
    return 1
  end end
end)
local ERR = macro(function(err)
  return quote
    C.printf("ERROR: %s\n", err)
    return 1
  end
end)

local terra exec()
  var store       = API.NewStore()
  var A           = store:A()
  var OUT         = store:OUT()

  A:beginload(4)
  A:loadrow( 0 )
  A:loadrow( 1 )
  A:loadrow( 2 )
  A:loadrow( 3 )
  A:endload()
  CHECK_ERR(store)

  store:self_join()
  CHECK_ERR(store)

  var n_OUT       = OUT:getsize()
  C.printf("got %d output rows\n", n_OUT)
  if n_OUT ~= 3 then
    store:destroy()
    C.printf("expected 3 contacts; got %d\n", n_OUT)
    ERR("bad result count")
  end
  var lhs, rhs  = OUT:lhs():read_lock(), OUT:rhs():read_lock()
  for k=0,3 do
    C.printf("row %d (lhs,rhs): %d %d\n", k, lhs[k], rhs[k] )
  end
  OUT:lhs():read_unlock(); OUT:rhs():read_unlock()

  escape if GPU_ON then emit quote
    C.printf("running gpu version now\n")
    store:self_join_GPU()
    CHECK_ERR(store)

    var n_OUT       = OUT:getsize()
    C.printf("got %d output rows\n", n_OUT)
    if n_OUT ~= 3 then
      store:destroy()
      C.printf("expected 3 contacts; got %d\n", n_OUT)
      ERR("bad result count")
    end
    lhs, rhs  = OUT:lhs():read_lock(), OUT:rhs():read_lock()
    for k=0,3 do
      C.printf("row %d (lhs,rhs): %d %d\n", k, lhs[k], rhs[k] )
    end
    OUT:lhs():read_unlock(); OUT:rhs():read_unlock()
  end end end

  store:destroy()

  return 0
end

test.eq(exec(), 0)

