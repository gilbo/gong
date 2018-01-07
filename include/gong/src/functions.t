
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
    _argtypes = assert(params.argtypes),
    _rettype  = assert(params.rettype),
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
end

function Function:argtypes()    return self._argtypes:copy()  end
function Function:rettype()     return self._rettype          end


-------------------------------------------------------------------------------
-- Joins
-------------------------------------------------------------------------------

local Join        = {}
Join.__index      = Join

local function NewJoin(params)
  assert(type(params) == 'table')
  local j = setmetatable({
    _name     = assert(params.name),
    _argtypes = assert(params.argtypes),
    _ast      = assert(params.ast),
  }, Join)
  return j
end
Exports.NewJoin = NewJoin

local function is_join(obj) return getmetatable(obj) == Join end
Exports.is_join   = is_join

function Join:getname()
  return self._name
end
function Join:setname(nm)
  self._name = nm
end

function Join:argtypes()    return self._argtypes:copy()  end


-------------------------------------------------------------------------------
-- Built-Ins
-------------------------------------------------------------------------------

local BuiltIn     = {}
BuiltIn.__index   = BuiltIn

local function NewBuiltIn(params)
  assert(type(params) == 'table')
  local b = setmetatable({
    _name       = assert(params.name),
    _typecheck  = assert(params.typecheck),
    _codegen    = assert(params.codegen),
  }, BuiltIn)
  return b
end
Exports.NewBuiltIn = NewBuiltIn

local function is_builtin(obj) return getmetatable(obj) == BuiltIn end
Exports.is_builtin  = is_builtin

function BuiltIn:getname()
  return self._name
end








