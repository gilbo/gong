
local Exports = {}
package.loaded["gong.src.acc_scan"] = Exports

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

local AccStructs        = require 'gong.src.acc_structs'
local is_spatial_index  = AccStructs.is_spatial_index

local newlist           = terralib.newlist

-------------------------------------------------------------------------------
--[[                           Helper Functions                            ]]--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--[[                    Spatial Index Structure Objects                    ]]--
-------------------------------------------------------------------------------

-- effectively no indexing
local Scan_Index      = setmetatable({},AccStructs.SpatialIndex)
Scan_Index.__index    = Scan_Index

function Scan_Index:__newindex(key, val)
  error("Cannot assign members to Scan_Index object",2)
end

function is_scan_index(obj)
  return getmetatable(obj) == Scan_Index
end

local scan_arg_msg = [[
new_scan_index() expects named arguments
{
  table       = gong data table to index
}
]]
local scan_index_cache = {}
local function new_scan_index(args)
  if not args.table
  then
    error(scan_arg_msg, 2)
  end
  if not is_table(args.table) then
    error("expected 'table' to be a gong data table\n"..scan_arg_msg, 2)
  end

  if scan_index_cache[args.table] then
    return scan_index_cache[args.table] end

  local scanidx = AccStructs.NewSpatialIndexObj({
    _table          = args.table,
  }, Scan_Index)

  scan_index_cache[args.table] = scanidx
  return scanidx
end

function Scan_Index:name() return self._table:name().."_Scan_Index" end



-------------------------------------------------------------------------------
--[[                           Traversal Objects                           ]]--
-------------------------------------------------------------------------------

local Scan_Scan_Traversal     = setmetatable({},AccStructs.Traversal)
Scan_Scan_Traversal.__index   = Scan_Scan_Traversal

function Scan_Scan_Traversal:__newindex(key, val)
  error("Cannot assign members to Scan_Scan_Traversal object",2)
end

local scanscan_arg_msg = [[
new_scan_scan_traversal() expects named arguments
{
  left        = gong spatial index to traverse (any type of index)
  right       = gong spatial index to traverse (any type of index)
}
]]
local function new_scan_scan_traversal(args)
  if not args.left or
     not args.right
  then
    error(scanscan_arg_msg, 2)
  end
  if not is_spatial_index(args.left) or not is_spatial_index(args.right) then
    error("expected 'left' and 'right' to be spatial index objects\n"..
          scanscan_arg_msg, 2)
  end

  local scanscan_trav = AccStructs.NewTraversalObj({
    _left           = args.left,
    _right          = args.right,
    _cpu_enabled    = true,
    _gpu_enabled    = true,
  }, Scan_Scan_Traversal)

  return scanscan_trav
end

local function is_scan_scan_traversal(obj)
  return getmetatable(obj) == Scan_Scan_Traversal
end



-------------------------------------------------------------------------------
--[[                                  Scan                                 ]]--
-------------------------------------------------------------------------------


function Scan_Index:_INTERNAL_StructLayout(StoreAPI)
  local CACHE   = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.SCAN then return CACHE.SCAN end

  local struct SCAN {}
  terra SCAN:init() end
  terra SCAN:destroy() end

  CACHE.SCAN = SCAN
  return SCAN
end

function Scan_Index:_INTERNAL_do_invalidate(idxptr, size_invalid,data_invalid)
  return quote end
end

function Scan_Index:_INTERNAL_PreJoinUpdate( StoreAPI,
                                             name, storeptr, idxptr,
                                             gpu_tblptr, gpu_globptr )
  return quote end
end

function Scan_Index:_INTERNAL_Construct_Functions(StoreAPI)
  local CACHE   = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.FUNCTIONS_BUILT then return end

  CACHE.FUNCTIONS_BUILT       = true
end

function Scan_Scan_Traversal:_INTERNAL_PreJoinUpdate(
  L_API, R_API, name, storeptr, idxptr0, idxptr1, gpu_tblptr, gpu_globptr
)
  return quote end
end

function Scan_Scan_Traversal:_INTERNAL_Split_LoopGen(L_API, R_API, for_gpu)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  local CACHE   = self:_INTERNAL_get_CACHE(L_API)

  if for_gpu then
    return CACHE.Scan_Scan_GPU_loopgen
  else
    return CACHE.Scan_Scan_CPU_loopgen
  end
end

function Scan_Scan_Traversal:_INTERNAL_LoopGen(StoreAPI, for_gpu)
  return self:_INTERNAL_Split_LoopGen(StoreAPI, StoreAPI, for_gpu)
end

function Scan_Scan_Traversal:_INTERNAL_Construct_Functions(API_0, API_1)
  local CACHE   = self:_INTERNAL_get_CACHE(API_0)
  if CACHE.FUNCTIONS_BUILT then return end

  local tbl0type = T.row(self._left._table)
  local tbl1type = T.row(self._right._table)
  
  local function Scan_Scan_CPU_loopgen( storeptr, is_self_join,
                                        idxptr0, idxptr1,
                                        row0sym, row1sym, args,
                                        bodycode )
    if is_self_join and API_0 == API_1 then
      return quote
        [ API_0:SelfScan( storeptr, tbl0type,
                             row0sym, row1sym, bodycode ) ]
      end
    else
      if is_self_join then
        local body_inner = bodycode
        bodycode = quote if row0sym <= row1sym then [body_inner] end end
      end
      return API_0:Scan( storeptr, tbl0type, row0sym,
                API_1:Scan( storeptr, tbl1type, row1sym,
                  bodycode
                ))
    end
  end
  CACHE.Scan_Scan_CPU_loopgen = Scan_Scan_CPU_loopgen

  local function Scan_Scan_GPU_loopgen( name, storeptr, is_self_join,
                                        gpu_tblptr, gpu_globptr,
                                        idxptr0, idxptr1,
                                        row0sym, row1sym, args, bodycode)
    if is_self_join and API_0 == API_1 then
      return API_0:GPU_SelfScan( name, storeptr,
                                    gpu_tblptr, gpu_globptr,
                                    tbl0type, row0sym, row1sym,
                                    args, bodycode )
    elseif API_0 == API_1 then -- and NOT is_self_join
      return API_0:GPU_DoubleScan( name, storeptr,
                                      gpu_tblptr, gpu_globptr,
                                      tbl0type, row0sym, tbl1type, row1sym,
                                      args, bodycode )
    else
      -- complicated double join situation...
      error("TODO: implement multi-\"api\" GPU joins...")
    end
  end
  CACHE.Scan_Scan_GPU_loopgen = Scan_Scan_GPU_loopgen

  CACHE.FUNCTIONS_BUILT       = true
end











Exports.is_scan_scan_traversal  = is_scan_scan_traversal


Exports.scan_scan_traversal = new_scan_scan_traversal


Exports.is_scan_index = is_scan_index
Exports.scan_index          = new_scan_index

