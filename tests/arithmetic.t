import 'gong'
local test    = require 'tests.test'

local G       = gong.stdlib
local assert  = G.assert

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
end

int32arith:test_exec()

local gong function uint64arith()
  var x : G.uint64 = 42
  var y : G.uint64 = x
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

  assert(-x > 0) -- weird but true cause unsigned
end

uint64arith:test_exec()

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
end

floatarith:test_exec()

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
end

doublearith:test_exec()

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
end

coercearith:test_exec()

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
end

booltests:test_exec()

--int32arith:printstats()
--uint64arith:printstats()
--floatarith:printstats()
--doublearith:printstats()

------------------------------------------------------------------------------

------------------------------------------------------------------------------
