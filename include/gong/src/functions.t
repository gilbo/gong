
local Exports = {}
package.loaded["gong.src.functions"] = Exports


local T         = require 'gong.src.types'

local newlist   = terralib.newlist


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

local Function    = {}
Function.__index  = Function

local function NewFunction(params)
  assert(type(params) == 'table')
  local f = setmetatable({
    _name     = assert(params.name),
    _ast      = assert(params.ast),
  }, Function)
  return f
end
Exports.NewFunction = NewFunction

local function is_function(obj) return getmetatable(obj) == Function end
Exports.is_function = is_function

function Function:getname()
  return self._name
end

function Function:setname(nm)
  self._name = nm
  assert(false, 'TODO')
end
