
import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib

------------------------------------------------------------------------------


local A   = G.NewTable('A')
local OUT = G.NewTable('OUT')
A:NewField('id', G.int32)
OUT:NewField('lhs', A)
OUT:NewField('rhs', A)
local gong join self_join( lhs : A, rhs : A )
  where lhs ~= rhs
do
  emit { lhs=lhs, rhs=rhs } in OUT
end


------------------------------------------------------------------------------


local API = G.CompileLibrary {
  tables        = {},
  joins         = {self_join},
  terra_out     = true,
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

  A:beginload(2)
  A:loadrow( 0 )
  A:loadrow( 1 )
  A:endload()
  CHECK_ERR(store)

  for k=0,10 do
    store:self_join()
    CHECK_ERR(store)
  end

  var n_OUT       = OUT:getsize()
  C.printf("got %d output rows\n", n_OUT)
  if n_OUT ~= 1 then
    store:destroy()
    ERR("expected 1 contact; got another number")
  end

  store:destroy()
  return 0
end

test.eq(exec(), 0)


