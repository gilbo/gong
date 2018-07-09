import 'gong'
local test    = require 'tests.test'

local G       = gong.stdlib
local assert  = G.assert

------------------------------------------------------------------------------



local BIG_CONST_ONE = 0x1ULL

-- THIS NEEDS SOME FIXING; breaks if you pass a cdata value in
local bigconst      = G.Constant(G.uint64, 0x31)

local gong function test1()
  var x : G.uint64 = 0x0ULL
  var y : G.uint64 = x
  assert(x-y == G.uint64(0))

  var one     = BIG_CONST_ONE

  var high1   = G.lshift(one, 63)
  assert(G.int64(high1) == -G.int64(high1))
  assert(0 == high1 + high1)
  assert(G.int64(high1) < 0)

  var neg1    = G.uint64(-G.int64(one))
  assert(high1 == G.bitand(neg1,high1))
  assert(one == G.bitand(neg1,one))

  var three   = 0x3ULL
  var two     = 0x2ULL
  assert( two == G.bitxor(three,one) )

  assert( x == G.bitnot(neg1) )

  assert( bigconst == 0x31ULL )

  return 0
end


local gong function test2()
  var x : G.uint64 = 0x0000010000000000ULL
  var y : G.uint64 = 0x1010101010101010ULL
  var lo,hi     = G.mul_lohi(x,y)
  var slo, shi  = G.mul_lohi(y,x)
  assert( lo ==      0x1010100000000000ULL)
  assert( hi ==      0x0000001010101010ULL)
  assert( lo == slo and hi == shi )

  -- force some non-trivial carrying between
  -- the lo and hi 64-bits of the result based on
  -- behavior at the lo and hi 32-bit boundary of the operands
  -- Bad hand-implementations are likely to miss the
  -- carrying behavior
  var a       = 0x0000000180000000ULL
  var b       = 0x0000000180000000ULL
  lo, hi  = G.mul_lohi(a,b)
  assert( lo == 0x4000000000000000ULL )
  assert( hi == 0x0000000000000002ULL )

  return 0
end



------------------------------------------------------------------------------

local A = G.NewTable('A')
local B = G.NewTable('B')
A:NewField('id', G.int32)
B:NewField('id', G.int32)
local gong join alltests( a : A, b : B )
do
  var x0    = test1()
  var y0    = test2()
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

