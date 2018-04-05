import 'gong'
local test    = require 'tests.test'

local G       = gong.stdlib

------------------------------------------------------------------------------


local gong function select_g( b : G.bool, v0 : G.uint32, v1 : G.uint32 )
  return b? v0 else v1
end

test.fail(function()
  local gong function letdo( a : G.uint32, b : G.uint32 )
    return (let
              var x = a*a
              b = 32
            in x+b end) + a
  end
end, "Cannot assign to %'b%' because it was defined outside of this")

local gong function letdo( a : G.uint32, b : G.uint32 )
  return (let
            var x = a*a
            var b = 32
          in x+b end) + a
end


------------------------------------------------------------------------------


local A = G.NewTable('A')
A:NewField('id', G.int32)
local gong join alltests( a : A, aa : A )
  -- no select filter
do
  var x0    = select_g(false, 4, 9)
  var x1    = letdo( 3, 2 )
end

local API = G.CompileLibrary {
  tables        = {},
  joins         = {alltests},
  terra_out     = true,
}

local terra exec()
  var store     = API.NewStore()
  var A         = store:A()

  A:beginload(1)
    A:loadrow(0)
  A:endload()

  store:alltests()

  store:destroy()
end

exec()

