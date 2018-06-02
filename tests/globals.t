import 'gong'
local test    = require 'tests.test'

local G       = gong.stdlib
local assert  = G.cassert
local C       = terralib.includecstring [[
#include "stdio.h"
]]

------------------------------------------------------------------------------

local boxT            = G.record { {'lo', G.vec3f}, {'hi', G.vec3f} }
local I               = math.huge

local count           = G.Global('count', G.int32, 2) -- init val 2
local bounds          = G.Global('bounds', boxT, { lo = { I, I, I},
                                                   hi = {-I,-I,-I} })

------------------------------------------------------------------------------


local A = G.NewTable('A')
A:NewField('id', G.int32)
A:NewField('pos', G.vec3f)
local gong join incr_count( a : A, aa : A, inc : G.int32 )
  -- no select filter
do
  count += inc
end

local gong join fit_bounds( a : A, aa : A )
  -- no select filter
do
  bounds.lo min= a.pos
  bounds.lo min= aa.pos
  bounds.hi max= a.pos
  bounds.hi max= aa.pos
end

local API = G.CompileLibrary {
  tables        = {},
  joins         = { incr_count, fit_bounds },
  terra_out     = true,
}

local v3 = macro(function(x,y,z)
  return `[ G.vec3f:terratype() ]({ d=array(x,y,z) })
end)

local terra exec()
  var store     = API.NewStore()
  var A         = store:A()

  A:beginload(1)
    A:loadrow(0, v3(0f,0f,0f))
  A:endload()

  assert(store:count():read() == 2)
  store:incr_count(-3)
  assert(store:count():read() == -1)
  store:count():write(0)
  assert(store:count():read() == 0)

  A:beginload(3)
    A:loadrow(0, v3(    1f, -0.5f,    0f))
    A:loadrow(1, v3(    0f,    1f,    0f))
    A:loadrow(2, v3(    0f,    0f,    0.75f))
  A:endload()

  store:incr_count(1)
  assert(store:count():read() == 6)

  store:fit_bounds()
  var box = store:bounds():read()
  assert( box.lo.d[0] ==    0f and
          box.lo.d[1] == -0.5f and
          box.lo.d[2] ==    0f )
  assert( box.hi.d[0] ==    1f and
          box.hi.d[1] ==    1f and
          box.hi.d[2] == 0.75f )

  store:destroy()
end

exec()
