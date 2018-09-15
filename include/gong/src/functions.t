
local Exports = {}
package.loaded["gong.src.functions"] = Exports


local T           = require 'gong.src.types'
local AccStructs  = require 'gong.src.acc_structs'

local newlist     = terralib.newlist


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
    _subfuncs = newlist(),
    _effects  = newlist(),
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

function Function:_INTERNAL_getast()
  return self._ast
end
function Function:_INTERNAL_getsubfuncs()
  return self._subfuncs:copy()
end
function Function:_INTERNAL_geteffects()
  return self._effects:copy()
end

-------------------------------------------------------------------------------
-- Joins
-------------------------------------------------------------------------------

local Join        = {}
Join.__index      = Join

local function NewJoin(params)
  assert(type(params) == 'table')
  local j = setmetatable({
    _name       = assert(params.name),
    _scantypes  = assert(params.scantypes),
    _argtypes   = assert(params.argtypes),
    _ast        = assert(params.ast),
    _subfuncs   = newlist(),
    _effects    = newlist(),
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
function Join:scantypes()   return self._scantypes:copy() end

function Join:set_cpu_traversal(obj)
  if self._cpu_traversal then
    error("CPU traversal for join already set", 2)
  end
  if not AccStructs.is_traversal(obj) then
    error("expected traversal as argument", 2)
  end
  if not obj:for_cpu() then
    error("expected a CPU-valid traversal", 2)
  end
  self._cpu_traversal = obj
end
function Join:get_cpu_traversal()
  return self._cpu_traversal
end
function Join:set_gpu_traversal(obj)
  if self._gpu_traversal then
    error("GPU traversal for join already set", 2)
  end
  if not AccStructs.is_traversal(obj) then
    error("expected traversal as argument", 2)
  end
  if not obj:for_gpu() then
    error("expected a GPU-valid traversal", 2)
  end
  self._gpu_traversal = obj
end
function Join:get_gpu_traversal()
  return self._gpu_traversal
end

function Join:_INTERNAL_getast()
  return self._ast
end
function Join:_INTERNAL_getsubfuncs()
  return self._subfuncs:copy()
end
function Join:_INTERNAL_geteffects()
  return self._effects:copy()
end

-------------------------------------------------------------------------------
-- Built-Ins
-------------------------------------------------------------------------------

local BuiltIn     = {}
BuiltIn.__index   = BuiltIn

local function NewBuiltIn(params)
  assert(type(params) == 'table')
  local luafunc = params.luafunc or
                  function() error("Cannot call from Lua code", 3) end
  local b = setmetatable({
    _name         = assert(params.name),
    _luafunc      = luafunc,
    _typecheck    = assert(params.typecheck),
    _effectcheck  = assert(params.effectcheck),
    _codegen      = assert(params.codegen),
  }, BuiltIn)
  return b
end
Exports.NewBuiltIn = NewBuiltIn

local function is_builtin(obj) return getmetatable(obj) == BuiltIn end
Exports.is_builtin  = is_builtin

function BuiltIn:__call(...)
  return self._luafunc(...)
end

function BuiltIn:getname()
  return self._name
end








