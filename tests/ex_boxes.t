
local test            = require 'tests.test'

local Pathname        = (require "gong.tools.pathname").Pathname

local ex_dir          = (Pathname.gong_root:abspath()..
                                        '../examples'):cleanpath()
local cmd             = 'cd '..tostring(ex_dir..'boxes')..'; '..
                        'make'

test.eq(os.execute(cmd), 0)
