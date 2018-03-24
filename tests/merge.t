
import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib

------------------------------------------------------------------------------


local A   = G.NewTable('A')
              :NewField('id', G.int32)
local B   = G.NewTable('B')
              :NewField('id', G.int32)
local OUT = G.NewTable('OUT')
              :NewField('a', A)
              :NewField('b', B)
              :SetPrimaryKey('a','b')
              :NewField('count', G.int32)

local gong join re_join( a : A, b : B, mul : G.int32, off : G.int32 )
  where mul*a.id + off == b.id
do
  merge x in OUT do
    x.count = x.count + 1
  else emit { count=1 } in OUT end
end


------------------------------------------------------------------------------


local API = G.CompileLibrary {
  tables        = {},
  joins         = {re_join},
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
  var OUT         = store:OUT()

  var N = 6
  A:beginload(N)
  for k=0,N do
    A:loadrow( k )
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
    store:re_join(1, 0)
    CHECK_ERR(store)
    var n_OUT     = OUT:get_n_rows()
    var ALLOC     = OUT:get_n_alloc()
    if n_OUT ~= 6 then ERR("expected 6 contacts; got %d", n_OUT) end
    for k = 0,ALLOC do if OUT:is_live():read(k) then
      var a, b, c = OUT:a():read(k), OUT:b():read(k), OUT:count():read(k)
      if c ~= 1 then ERR("expected all counts to be 1, but got %d", c) end
    end end
  end
  C.printf("  Join 1 done\n")
  -- repeat the same join for counts of 2 everywhere
  do
    store:re_join(1, 0)
    CHECK_ERR(store)
    var n_OUT     = OUT:get_n_rows()
    var ALLOC     = OUT:get_n_alloc()
    if n_OUT ~= 6 then ERR("expected 6 contacts; got %d", n_OUT) end
    for k = 0,ALLOC do if OUT:is_live():read(k) then
      var a, b, c = OUT:a():read(k), OUT:b():read(k), OUT:count():read(k)
      if c ~= 2 then ERR("expected all counts to be 2, but got %d", c) end
    end end
  end
  C.printf("  Join 2 done\n")
  -- now do the join with a shift so that only one row persists
  do
    store:re_join(2, -2)
    CHECK_ERR(store)
    -- expected pairs (1,0),(2,2),(3,4)
    var n_OUT     = OUT:get_n_rows()
    var ALLOC     = OUT:get_n_alloc()
    if n_OUT ~= 3 then ERR("expected 3 contacts; got %d", n_OUT) end
    for k = 0,ALLOC do if OUT:is_live():read(k) then
      var a, b, c = OUT:a():read(k), OUT:b():read(k), OUT:count():read(k)
      if a==2 and b==2 then
        if c ~= 3 then
          ERR("expected (a=2,b=2) to have count 3, but got %d", c) end
      else
        if c ~= 1 then
          ERR("(a=%d,b=%d) has count=%d rather than count 1", a, b, c) end
        if not(a == 1 and b == 0) and not(a == 3 and b == 4) then
          ERR("got unexpected output row (a=%d,b=%d)", a, b) end
      end
    end end
  end
  C.printf("  Join 3 done\n")

  store:destroy()
  return 0
end

test.eq(exec(), 0)

