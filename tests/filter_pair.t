import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib

------------------------------------------------------------------------------


local A   = G.NewTable('A')
              :NewField('id', G.int32)
              :NewField('hit', G.bool)
local B   = G.NewTable('B')
              :NewField('id', G.int32)

local gong join filter_join(a : A, b : B, mul : G.int32, off : G.int32 )
  where mul*a.id + off == b.id
do
  a.hit or= true
end


------------------------------------------------------------------------------


local API = G.CompileLibrary {
  tables        = {},
  joins         = {filter_join},
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
local ERR = macro(function(err, ...)
  err = err:asvalue()
  local args = terralib.newlist{...}
  return quote
    C.printf(["ERROR: "..err.."\n"], [args])
    return 1
  end
end)

local terra exec()
  var store       = API.NewStore()
  var A           = store:A()
  var B           = store:B()
  --var OUT         = store:OUT()

  var N = 6
  A:beginload(N)
  for k=0,N do
    A:loadrow( k, false )
  end
  A:endload()
  B:beginload(N)
  for k=0,N do
    B:loadrow( k )
  end
  B:endload()
  CHECK_ERR(store)
  C.printf("  Load done\n")
  do
    store:filter_join(1, 0)
    CHECK_ERR(store)
    var n_A     = A:getsize()
    if n_A ~= 6 then ERR("expected 6 vals; got %d", n_A) end
    for k = 0,n_A do
      var a, b = A:id():read(k), A:hit():read(k)
      if not b then ERR("expected all hits, but got miss on %d", a) end
    end
  end
  C.printf("  Join 1 done\n")
  -- repeat the same join, get same result
  do
    store:filter_join(1, 0)
    CHECK_ERR(store)
    var n_A     = A:getsize()
    if n_A ~= 6 then ERR("expected 6 vals; got %d", n_A) end
    for k = 0,n_A do
      var a, b = A:id():read(k), A:hit():read(k)
      if not b then ERR("expected all hits, but got miss on %d", a) end
    end
  end
  C.printf("  Join 2 done\n")

  -- "Clear"
  A:beginload(N)
  for k=0,N do
    A:loadrow( k, false )
  end
  A:endload()

  -- now do the join with a shift so that only one row persists
  do
    store:filter_join(2, -2)
    CHECK_ERR(store)
    -- expected pairs (1,0),(2,2),(3,4)
    var n_A     = A:getsize()
    if n_A ~= 6 then ERR("expected 6 vals; got %d", n_A) end
    for k = 0,n_A do
      var a, b = A:id():read(k), A:hit():read(k)
      if (a == 0 or a == 4 or a == 5) then
          if b then ERR("expected miss on 0,4,5 but got hit on %d", a) end
      else
          if not b then ERR("expected hit on 1,2,3 but got hit on %d", a) end
      end
    end
  end
  C.printf("  Join 3 done\n")

  store:destroy()
  return 0
end

test.eq(exec(), 0)

