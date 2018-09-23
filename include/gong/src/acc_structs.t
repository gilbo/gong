
local Exports = {}
package.loaded["gong.src.acc_structs"] = Exports

-------------------------------------------------------------------------------

local T             = require 'gong.src.types'
local is_type       = T.is_type

local Util          = require 'gong.src.util'
local is_pos_int    = Util.is_pos_int

local StdContainers = require 'gong.src.stdcontainers'
local vector        = StdContainers.vector
local stack         = StdContainers.stack

local C             = require 'gong.src.c'
local assert        = C.assert

local Schemata      = require 'gong.src.schemata'
local is_table      = Schemata.is_table

local Functions     = require 'gong.src.functions'

local EC            = nil
local function lazy_load_EC()
  if not EC then
    EC              = require 'gong.src.effectcheck'
  end
end

local newlist       = terralib.newlist

-------------------------------------------------------------------------------
--[[                           Helper Functions                            ]]--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--[[                    Spatial Index Structure Objects                    ]]--
-------------------------------------------------------------------------------

local SpatialIndex    = {}
SpatialIndex.__index  = SpatialIndex
Exports.SpatialIndex  = SpatialIndex

local function is_spatial_index(obj)
  return getmetatable(getmetatable(obj)) == SpatialIndex
end
Exports.is_spatial_index = is_spatial_index

function SpatialIndex:table() return self._table end

function SpatialIndex:_INTERNAL_getsubfuncs() return self._subfuncs:copy() end
function SpatialIndex:_INTERNAL_geteffects()  return self._effects:copy()  end

local function NewSpatialIndexObj(obj,Class)
  local subf      = {}
  local eff       = {}
  if obj._subfuncs then for _,f in ipairs(obj._subfuncs) do
    subf[f]       = true
    for _,sf in ipairs(f:_INTERNAL_getsubfuncs()) do
      subf[sf]    = true
    end
    for _,e in ipairs(f:_INTERNAL_geteffects()) do
      eff[e]      = true
    end
  end end

  obj._subfuncs   = newlist()
  obj._effects    = newlist()
  obj._CACHE      = {}

  lazy_load_EC()
  local is_read   = EC.Effects.Read.check
  local is_read_g = EC.Effects.ReadG.check

  local depends_on        = {}
  for f,_ in pairs(subf) do obj._subfuncs:insert(f) end
  for e,_ in pairs(eff)  do
    obj._effects:insert(e)
    if is_read(e) then
      local tbl           = e.src:table()
      local fld           = tbl:fields(e.path[1].name)
      depends_on[fld]     = true
      depends_on[tbl]     = true
    elseif is_read_g(e) then
      local glob  = e.src
      depends_on[glob]    = true
    else assert(not e:touches_memory(),
                'INTERNAL: expected no non-read memory effects') end
  end
  depends_on[obj._table]  = true
  obj._depends_on = depends_on

  return setmetatable(obj,Class)
end

Exports.NewSpatialIndexObj    = NewSpatialIndexObj

function SpatialIndex:_INTERNAL_get_CACHE(StoreAPI)
  if self._CACHE[StoreAPI] then return self._CACHE[StoreAPI] end
  self._CACHE[StoreAPI] = {}
  return self._CACHE[StoreAPI]
end

function SpatialIndex:_INTERNAL_depends_on(obj)
  return self._depends_on[obj]
end


-------------------------------------------------------------------------------
--[[                           Traversal Objects                           ]]--
-------------------------------------------------------------------------------

local Traversal       = {}
Traversal.__index     = Traversal
Exports.Traversal     = Traversal

local function is_traversal(obj)
  return getmetatable(getmetatable(obj)) == Traversal
end
Exports.is_traversal  = is_traversal

function Traversal:for_cpu()  return self._cpu_enabled  end
function Traversal:for_gpu()  return self._gpu_enabled  end

function Traversal:left()     return self._left         end
function Traversal:right()    return self._right        end

function Traversal:_INTERNAL_getsubfuncs() return self._subfuncs:copy() end
function Traversal:_INTERNAL_geteffects()  return self._effects:copy()  end

local function NewTraversalObj(obj,Class)
  local subf      = {}
  local eff       = {}
  local depends_on  = {}
  if obj._subfuncs then for _,f in ipairs(obj._subfuncs) do
    subf[f]       = true
    for _,sf in ipairs(f:_INTERNAL_getsubfuncs()) do
      subf[sf]    = true
    end
    for _,e in ipairs(f:_INTERNAL_geteffects()) do
      eff[e]      = true
    end
  end end

  obj._subfuncs   = newlist()
  obj._effects    = newlist()
  obj._CACHE      = {}

  for f,_ in pairs(subf) do obj._subfuncs:insert(f) end
  for e,_ in pairs(eff)  do obj._effects:insert(e)  end

  return setmetatable(obj,Class)
end

Exports.NewTraversalObj       = NewTraversalObj

function Traversal:_INTERNAL_get_CACHE(StoreAPI)
  if self._CACHE[StoreAPI] then return self._CACHE[StoreAPI] end
  self._CACHE[StoreAPI] = {}
  return self._CACHE[StoreAPI]
end




