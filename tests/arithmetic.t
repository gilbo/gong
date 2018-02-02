import 'gong'
local test    = require 'tests.test'

local G       = gong.stdlib
local assert  = --G.assert

------------------------------------------------------------------------------

local gong function int32arith()
  var x = 42
  var y : G.int32 = x
  assert(x-y == 0)
  assert(x+y == 84)

  x = 3
  y = 5
  assert(x*y == 15)
  assert(x/y == 0)
  assert(y/x == 1)

  assert((-x) * (-y) == x * y)
  assert(x <= y)
  assert(x < y)
  assert(y > x)
  assert(y >= x)
  assert(x ~= y)

  return 0
end

local gong function uint64arith()
  var x : G.uint64 = G.uint64(42)
  var y : G.uint64 = x
  assert(x-y == G.uint64(0))
  assert(x+y == G.uint64(84))

  x = G.uint64(3)
  y = G.uint64(5)
  assert(x*y == G.uint64(15))
  assert(x/y == G.uint64(0))
  assert(y/x == G.uint64(1))

  --assert((-x) * (-y) == x * y)
  assert(x <= y)
  assert(x < y)
  assert(y > x)
  assert(y >= x)
  assert(x ~= y)

  --assert(-x > G.uint64(0)) -- weird but true cause unsigned

  return 0
end

local gong function floatarith()
  var x = 42.0f
  var y : G.float = x
  assert(x-y == 0f)
  assert(x+y == 84f)

  x = 3.0f
  y = 5.0f
  assert(x*y == 15f)
  assert(x/y == 3.0f/5.0f)
  assert(y/x == 5.0f/3.0f)

  assert((-x) * (-y) == x * y)
  assert(x <= y)
  assert(x < y)
  assert(y > x)
  assert(y >= x)
  assert(x ~= y)

  return 0.0f
end

local gong function doublearith()
  var x = 42.0
  var y : G.double = x
  assert(x-y == 0)
  assert(x+y == 84)

  x = 3.0
  y = 5.0
  assert(x*y == 15)
  assert(x/y == 3.0/5.0)
  assert(y/x == 5.0/3.0)

  assert((-x) * (-y) == x * y)
  assert(x <= y)
  assert(x < y)
  assert(y > x)
  assert(y >= x)
  assert(x ~= y)

  return 0.0
end

local gong function coercearith()
  var x = 42
  var y : G.double = x
  assert(x-y == 0)
  assert(x+y == 84)

  x = 3
  y = 5
  assert(x*y == 15)
  assert(x/y == 3.0/5.0)
  assert(y/x == 5.0/3.0)

  assert((-x) * (-y) == x * y)
  assert(x <= y)
  assert(x < y)
  assert(y > x)
  assert(y >= x)
  assert(x ~= y)

  return 0
end

local gong function booltests()
  var x = true
  var y : G.bool = x
  assert(x and y == true)
  assert(x or y == true)
  assert(x == y)

  x = true
  y = false
  assert( (not x and not y) == not (x or y) )
  assert(x ~= y)

  return false
end

local A = G.NewTable('A')
local B = G.NewTable('B')
A:NewField('id', G.int32)
B:NewField('id', G.int32)
local gong join alltests( a : A, b : B )
do
  var x0    = int32arith()
  var x1    = uint64arith()
  var x2    = floatarith()
  var x3    = doublearith()
  var x4    = coercearith()
  var x5    = booltests()
end

local API = G.CompileLibrary {
  tables        = {},
  joins         = {alltests},
  terra_out     = true,
}

local terra exec()
  var store     = API.NewStore()
  var A         = store:A()
  var B         = store:B()

  A:beginload(1)
    A:loadrow(0)
  A:endload()

  B:beginload(1)
    B:loadrow(0)
  B:endload()

  store:alltests()

  store:destroy()
end

exec()


------------------------------------------------------------------------------

------------------------------------------------------------------------------
