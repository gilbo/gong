import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib

local GPU_ON = not not terralib.cudacompile

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
local ERR = macro(function(err, ...)
  err = err:asvalue()
  local args = terralib.newlist{...}
  return quote
    C.printf(["ERROR: "..err.."\n"], [args])
    return 1
  end
end)

local function gen_exec(use_gpu)
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
      escape if use_gpu then emit quote
        store:filter_join_GPU(1, 0)
      end else emit quote
        store:filter_join(1, 0)
      end end end
      CHECK_ERR(store)
      var n_A     = A:getsize()
      if n_A ~= 6 then ERR("expected 6 vals; got %d", n_A) end
      var a, b = A:id():read_lock(), A:hit():read_lock()
      for k = 0,n_A do
        if not b[k] then
          ERR("expected all hits, but got miss on %d", a[k]) end
      end
      A:id():read_unlock(); A:hit():read_unlock()
    end
    C.printf("  Join 1 done\n")
    -- repeat the same join, get same result
    do
      escape if use_gpu then emit quote
        store:filter_join_GPU(1, 0)
      end else emit quote
        store:filter_join(1, 0)
      end end end
      CHECK_ERR(store)
      var n_A     = A:getsize()
      if n_A ~= 6 then ERR("expected 6 vals; got %d", n_A) end
      var a, b = A:id():read_lock(), A:hit():read_lock()
      for k = 0,n_A do
        if not b[k] then
          ERR("expected all hits, but got miss on %d", a[k]) end
      end
      A:id():read_unlock(); A:hit():read_unlock()
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
      escape if use_gpu then emit quote
        store:filter_join_GPU(2, -2)
      end else emit quote
        store:filter_join(2, -2)
      end end end
      CHECK_ERR(store)
      -- expected pairs (1,0),(2,2),(3,4)
      var n_A     = A:getsize()
      if n_A ~= 6 then ERR("expected 6 vals; got %d", n_A) end
      var a, b = A:id():read_lock(), A:hit():read_lock()
      for k = 0,n_A do
        if (a[k] == 0 or a[k] == 4 or a[k] == 5) then
            if b[k] then
              ERR("expected miss on 0,4,5 but got hit on %d", a[k]) end
        else
            if not b[k] then
              ERR("expected hit on 1,2,3 but got hit on %d", a[k]) end
        end
      end
      A:id():read_unlock(); A:hit():read_unlock()
    end
    C.printf("  Join 3 done\n")

    store:destroy()
    return 0
  end
  return exec
end

test.eq(gen_exec(false)(), 0)
if GPU_ON then
  test.eq(gen_exec(true)(), 0)
end

