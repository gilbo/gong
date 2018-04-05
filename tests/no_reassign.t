import 'gong'
local test    = require 'tests.test'

local G       = gong.stdlib

------------------------------------------------------------------------------


local gong function select_g( b : G.bool, v0 : G.uint32, v1 : G.uint32 )
  return b? v0 else v1
end

test.fail(function()
  local gong function letdo( a : G.uint32, b : G.uint32 )
    var r = let
              var x = a*a
              b = 32
            in x+b end
    return r + a
  end
end, 'Cannot assign')


------------------------------------------------------------------------------