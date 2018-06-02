
local Exports = {}
package.loaded["gong.src.global"] = Exports


local T         = require 'gong.src.types'
local Util      = require 'gong.src.util'
local is_id_str = Util.is_id_str

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

---------------------------

local Global      = {}
Global.__index    = Global

local function NewGlobal(name, typ, val)
  if not is_id_str(name) then
    error('expected global variable name as first arg', 2)
  end
  if not T.is_type(typ) then error('expected Gong type as second arg', 2) end
  if val and not T.check_luaval(val, typ) then
    error('value did not match type', 2) end
  if typ:has_rows() then
    error('cannot create constants using row types', 2) end
  local g = setmetatable({
    _name     = name,
    _luaval   = val,
    _type     = typ,
  }, Global)
  return g
end
Exports.NewGlobal = NewGlobal

local function is_global(obj) return getmetatable(obj) == Global end
Exports.is_global = is_global

function Global:name()
  return self._name
end

function Global:setname(nm)
  self._name = nm
end

function Global:initval()
  return self._luaval
end

function Global:type()
  return self._type
end



