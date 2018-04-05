
local Exports = {}
package.loaded["gong.src.global"] = Exports


local T         = require 'gong.src.types'

local newlist   = terralib.newlist


-------------------------------------------------------------------------------
-- Constants & Globals
-------------------------------------------------------------------------------

local Constant    = {}
Constant.__index  = Constant

local function NewConstant(typ, val)
  if not T.is_type(typ) then error('expected Gong type as first arg', 2) end
  if val == nil then error('expected value as second arg', 2) end
  if not T.check_luaval(val, typ) then
    error('value did not match type', 2) end
  if typ:has_rows() then
    error('cannot create constants using row types', 2) end
  local c = setmetatable({
    _luaval   = val,
    _type     = typ,
  }, Constant)
  return c
end
Exports.NewConstant = NewConstant

local function is_constant(obj) return getmetatable(obj) == Constant end
Exports.is_constant = is_constant

function Constant:getvalue()
  return self._luaval
end

function Constant:type()
  return self._type
end