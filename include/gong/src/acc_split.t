
local Exports = {}
package.loaded["gong.src.acc_split"] = Exports

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

local AccStructs    = require 'gong.src.acc_structs'
local is_spatial_index      = AccStructs.is_spatial_index
local is_traversal          = AccStructs.is_traversal

local newlist       = terralib.newlist

-------------------------------------------------------------------------------
--[[                           Helper Functions                            ]]--
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--[[                    Spatial Index Structure Objects                    ]]--
-------------------------------------------------------------------------------

-- effectively no indexing
local Split_Index     = setmetatable({},AccStructs.SpatialIndex)
Split_Index.__index   = Split_Index

function Split_Index:__newindex(key, val)
  error("Cannot assign members to Split_Index object",2)
end

function is_split_index(obj)
  return getmetatable(obj) == Split_Index
end

local split_arg_msg = [[
new_split_index() expects named arguments
{
  table       = gong data table to index
  index_A     = gong index on 'table'
  index_B     = gong index on 'table'
  split       = gong function: table -> bool   (true means A)
}
]]
local function new_split_index(args)
  if not args.table or
     not args.index_A or
     not args.index_B or
     not args.split
  then
    error(split_arg_msg, 2)
  end

  if not is_table(args.table) then
    error("expected 'table' to be a gong data table\n"..split_arg_msg, 2)
  end

  if not is_spatial_index(args.index_A) or
     args.index_A:table() ~= args.table
  then
    error("expected 'index_A' to be a spatial index on table '"..
          args.table:name().."'\n"..split_arg_msg, 2)
  end
  if not is_spatial_index(args.index_B) or
     args.index_B:table() ~= args.table
  then
    error("expected 'index_B' to be a spatial index on table '"..
          args.table:name().."'\n"..split_arg_msg, 2)
  end

  if is_split_index(args.index_A) or is_split_index(args.index_B) then
    error("TODO: support nested split indices")
  end

  if not Functions.is_function(args.split) or
     #args.split:argtypes() ~= 1 or
     args.split:argtypes()[1] ~= T.row(args.table) or
     args.split:rettype() ~= T.bool
  then
      error("expected 'split' to be a gong function of type\n"..
            "  table -> bool\n"..split_arg_msg, 2)
  end

  local subf        = newlist()
  subf:insertall(args.index_A._subfuncs)
  subf:insertall(args.index_B._subfuncs)

  local splitidx = AccStructs.NewSpatialIndexObj({
    _table          = args.table,
    _index_A        = args.index_A:_INTERNAL_clone(),
    _index_B        = args.index_B:_INTERNAL_clone(),
    _split          = args.split,

    _subfuncs       = subf,
  }, Split_Index)

  return splitidx
end

function Split_Index:name()     return self._table:name().."_Split_Index" end
function Split_Index:index_A()  return self._index_A    end
function Split_Index:index_B()  return self._index_B    end

function Split_Index:_INTERNAL_clone()
  local obj     = AccStructs.SpatialIndex._INTERNAL_clone(self)
  obj._index_A  = self._index_A:_INTERNAL_clone()
  obj._index_B  = self._index_B:_INTERNAL_clone()
  return obj
end


-------------------------------------------------------------------------------
--[[                           Traversal Objects                           ]]--
-------------------------------------------------------------------------------

local Split_Split_Traversal   = setmetatable({},AccStructs.Traversal)
Split_Split_Traversal.__index = Split_Split_Traversal

function Split_Split_Traversal:__newindex(key, val)
  error("Cannot assign members to Split_Split_Traversal object",2)
end

local splitsplit_arg_msg = [[
new_split_split_traversal() expects named arguments
{
  left        = gong spatial index to traverse
  right       = gong spatial index to traverse
  AA_traverse = gong traversal
  AB_traverse = gong traversal
  BA_traverse = gong traversal
  BB_traverse = gong traversal
}
]]
local split_idx_copy_warn = [[
Warning: sub-indices of a split index are cloned on construction.
         Use splitidx:index_A() and splitidx:index_B() to retreive.
]]
local function new_split_split_traversal(args)
  if not args.left or
     not args.right or
     not args.AA_traverse or
     not args.AB_traverse or
     not args.BA_traverse or
     not args.BB_traverse
  then
    error(splitsplit_arg_msg, 2)
  end
  if not is_split_index(args.left) or not is_split_index(args.right) then
    error("expected 'left' and 'right' to be Split_Index objects\n"..
          splitsplit_arg_msg, 2)
  end

  if not is_traversal(args.AA_traverse) or
     args.AA_traverse._left ~= args.left._index_A or
     args.AA_traverse._right ~= args.right._index_A
  then
    error("expected 'AA_traverse' to be a traversal over the sub-indices\n"..
          split_idx_copy_warn..splitsplit_arg_msg, 2)
  end
  if not is_traversal(args.AB_traverse) or
     args.AB_traverse._left ~= args.left._index_A or
     args.AB_traverse._right ~= args.right._index_B
  then
    error("expected 'AB_traverse' to be a traversal over the sub-indices\n"..
          split_idx_copy_warn..splitsplit_arg_msg, 2)
  end
  if not is_traversal(args.BA_traverse) or
     args.BA_traverse._left ~= args.left._index_B or
     args.BA_traverse._right ~= args.right._index_A
  then
    error("expected 'BA_traverse' to be a traversal over the sub-indices\n"..
          split_idx_copy_warn..splitsplit_arg_msg, 2)
  end
  if not is_traversal(args.BB_traverse) or
     args.BB_traverse._left ~= args.left._index_B or
     args.BB_traverse._right ~= args.right._index_B
  then
    error("expected 'BB_traverse' to be a traversal over the sub-indices\n"..
          split_idx_copy_warn..splitsplit_arg_msg, 2)
  end

  local cpu_enabled = args.AA_traverse._cpu_enabled and
                      args.AB_traverse._cpu_enabled and
                      args.BA_traverse._cpu_enabled and
                      args.BB_traverse._cpu_enabled
  local gpu_enabled = args.AA_traverse._gpu_enabled and
                      args.AB_traverse._gpu_enabled and
                      args.BA_traverse._gpu_enabled and
                      args.BB_traverse._gpu_enabled

  local subf        = {}
  for _,f in ipairs(args.AA_traverse._subfuncs) do subf[f] = true end
  for _,f in ipairs(args.AB_traverse._subfuncs) do subf[f] = true end
  for _,f in ipairs(args.BA_traverse._subfuncs) do subf[f] = true end
  for _,f in ipairs(args.BB_traverse._subfuncs) do subf[f] = true end
  local subfuncs    = newlist()
  for f,_ in pairs(subf) do subfuncs:insert(f) end

  local splitsplit_trav = AccStructs.NewTraversalObj({
    _left           = args.left,
    _right          = args.right,
    _trav_AA        = args.AA_traverse:_INTERNAL_clone(),
    _trav_AB        = args.AB_traverse:_INTERNAL_clone(),
    _trav_BA        = args.BA_traverse:_INTERNAL_clone(),
    _trav_BB        = args.BB_traverse:_INTERNAL_clone(),
    _cpu_enabled    = cpu_enabled,
    _gpu_enabled    = gpu_enabled,

    _subfuncs       = subfuncs,
  }, Split_Split_Traversal)

  return splitsplit_trav
end

local function is_split_split_traversal(obj)
  return getmetatable(obj) == Split_Split_Traversal
end



-------------------------------------------------------------------------------
--[[                                 Split                                 ]]--
-------------------------------------------------------------------------------

function Split_Index:_INTERNAL_A_API(StoreAPI)
  local CACHE       = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.A_API then return CACHE.A_API end
  CACHE.A_API = setmetatable({},{ __index = StoreAPI })
  return CACHE.A_API
end
function Split_Index:_INTERNAL_B_API(StoreAPI)
  local CACHE       = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.B_API then return CACHE.B_API end
  CACHE.B_API = setmetatable({},{ __index = StoreAPI })
  return CACHE.B_API
end

function Split_Index:_INTERNAL_StructLayout(StoreAPI, gpu_on)
  local CACHE       = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.SPLIT then return CACHE.SPLIT end
  local A_API       = self:_INTERNAL_A_API(StoreAPI)
  local B_API       = self:_INTERNAL_B_API(StoreAPI)

  local Layout_A    = self._index_A:_INTERNAL_StructLayout( A_API )
  local Layout_B    = self._index_B:_INTERNAL_StructLayout( B_API )
  local struct SPLIT {
    _subidx_A   : Layout_A
    _subidx_B   : Layout_B
    _is_stale   : bool
    _is_in_A    : vector(bool)
    _A_size     : uint32
    _B_size     : uint32
  }
  terra SPLIT:init()
    self._subidx_A:init()
    self._subidx_B:init()
    self._is_in_A:init()
    self._is_stale = true
  end
  terra SPLIT:destroy()
    self._is_in_A:destroy()
    self._subidx_B:destroy()
    self._subidx_A:destroy()
  end

  CACHE.SPLIT = SPLIT
  return SPLIT
end

-- delegate...
function Split_Index:_INTERNAL_do_invalidate(idxptr, size_invalid,data_invalid)
  local invalidate = newlist {
    self._index_A:_INTERNAL_do_invalidate(`&([idxptr]._subidx_A),
                                          size_invalid, data_invalid),
    self._index_B:_INTERNAL_do_invalidate(`&([idxptr]._subidx_B),
                                          size_invalid, data_invalid),
  }
  return quote
    [idxptr]._is_stale = true
    [invalidate]
  end
end

function Split_Index:_INTERNAL_PreJoinUpdate( StoreAPI,
                                              name, storeptr, idxptr,
                                              gpu_tblptr, gpu_globptr )
  return quote end
end

function Split_Index:_INTERNAL_Construct_Functions(StoreAPI)
  local this        = self
  local CACHE       = this:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.FUNCTIONS_BUILT then return end

  local A_API       = this:_INTERNAL_A_API(StoreAPI)
  local B_API       = this:_INTERNAL_B_API(StoreAPI)

  local RowTyp      = T.row(this._table)
  local row_t       = RowTyp:terratype()

  local splitfn     = StoreAPI:GetCPUFunction( self._split )

  local StorePtr    = &(StoreAPI:StoreTyp())

  local SPLIT       = CACHE.SPLIT
  terra SPLIT:update( storeptr : StorePtr )
    if not self._is_stale then return end

    var nA, nB    = [uint32](0), [uint32](0)
    var N         = [ StoreAPI:Size(storeptr, RowTyp) ]
    self._is_in_A:resize(N)
    var row       : row_t
    [ StoreAPI:Scan(storeptr, RowTyp, row, quote
        var in_A  = splitfn( storeptr, row )
        self._is_in_A(row) = in_A
        if in_A then nA = nA + 1
                else nB = nB + 1 end
      end) ]
    self._A_size  = nA
    self._B_size  = nB

    self._is_stale = false
    assert(nA + nB == N, 'INTERNAL: sanity')
  end

  -- over-ride sub-index API size functions
  A_API.Size = function(api, storeptr, tbltype)
    assert(tbltype == RowTyp, 'INTERNAL')
    local idxptr  = StoreAPI:IndexPtr(storeptr, this)
    return `[idxptr]._A_size
  end
  B_API.Size = function(api, storeptr, tbltype)
    assert(tbltype == RowTyp, 'INTERNAL')
    local idxptr  = StoreAPI:IndexPtr(storeptr, this)
    return `[idxptr]._B_size
  end

  -- over-ride scans
  A_API.Scan = function(api, storeptr, tbltype, rowsym, bodycode)
    assert(tbltype == RowTyp, 'INTERNAL')
    local idxptr  = StoreAPI:IndexPtr(storeptr, this)
    return quote
      var Avec    = [idxptr]._is_in_A
      [ StoreAPI:Scan(storeptr, RowTyp, rowsym, quote
          if Avec(rowsym) then [bodycode] end
        end) ]
    end
  end
  B_API.Scan = function(api, storeptr, tbltype, rowsym, bodycode)
    assert(tbltype == RowTyp, 'INTERNAL')
    local idxptr  = StoreAPI:IndexPtr(storeptr, this)
    return quote
      var Avec    = [idxptr]._is_in_A
      [ StoreAPI:Scan(storeptr, RowTyp, rowsym, quote
          if not Avec(rowsym) then [bodycode] end
        end) ]
    end
  end
  A_API.SelfScan = function(api, storeptr, tbltype, row0, row1, bodycode)
    assert(tbltype == RowTyp, 'INTERNAL')
    local idxptr  = StoreAPI:IndexPtr(storeptr, this)
    return quote
      var Avec    = [idxptr]._is_in_A
      [ StoreAPI:SelfScan(storeptr, RowTyp, row0, row1, quote
          if Avec(row0) and Avec(row1) then [bodycode] end
        end) ]
    end
  end
  B_API.SelfScan = function(api, storeptr, tbltype, row0, row1, bodycode)
    assert(tbltype == RowTyp, 'INTERNAL')
    local idxptr  = StoreAPI:IndexPtr(storeptr, this)
    return quote
      var Avec    = [idxptr]._is_in_A
      [ StoreAPI:SelfScan(storeptr, RowTyp, row0, row1, quote
          if not Avec(row0) and not Avec(row1) then [bodycode] end
        end) ]
    end
  end

  A_API.GPU_DoubleScan  = function(api, ...) error('TODO') end
  B_API.GPU_DoubleScan  = function(api, ...) error('TODO') end
  A_API.GPU_SelfScan    = function(api, ...) error('TODO') end
  B_API.GPU_SelfScan    = function(api, ...) error('TODO') end

  CACHE.FUNCTIONS_BUILT       = true
end


function Split_Split_Traversal:_INTERNAL_PreJoinUpdate(
  L_API, R_API, name, storeptr, idxptr0, idxptr1, gpu_tblptr, gpu_globptr
)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  assert(not gpu_tblptr, 'INTERNAL: CPU ONLY')

  local L_A_API   = self._left:_INTERNAL_A_API(L_API)
  local L_B_API   = self._left:_INTERNAL_B_API(L_API)
  local R_A_API   = self._right:_INTERNAL_A_API(R_API)
  local R_B_API   = self._right:_INTERNAL_B_API(R_API)

  local updates   = newlist {
    quote [idxptr0]:update(storeptr)
          [idxptr1]:update(storeptr) end,
    self._trav_AA:_INTERNAL_PreJoinUpdate(
          L_A_API, R_A_API, name, storeptr,
          `&([idxptr0]._subidx_A), `&([idxptr1]._subidx_A),
          gpu_tblptr, gpu_globptr ),
    self._trav_AB:_INTERNAL_PreJoinUpdate(
          L_A_API, R_B_API, name, storeptr,
          `&([idxptr0]._subidx_A), `&([idxptr1]._subidx_B),
          gpu_tblptr, gpu_globptr ),
    self._trav_BA:_INTERNAL_PreJoinUpdate(
          L_B_API, R_A_API, name, storeptr,
          `&([idxptr0]._subidx_B), `&([idxptr1]._subidx_A),
          gpu_tblptr, gpu_globptr ),
    self._trav_BB:_INTERNAL_PreJoinUpdate(
          L_B_API, R_B_API, name, storeptr,
          `&([idxptr0]._subidx_B), `&([idxptr1]._subidx_B),
          gpu_tblptr, gpu_globptr ),
  }
  return quote [updates] end
end

function Split_Split_Traversal:_INTERNAL_Split_LoopGen(L_API, R_API, for_gpu)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  local CACHE   = self:_INTERNAL_get_CACHE(L_API)

  assert(not for_gpu, 'INTERNAL: expect Split_Index on CPU only')
  return CACHE.Split_Split_CPU_loopgen
end

function Split_Split_Traversal:_INTERNAL_LoopGen(StoreAPI, for_gpu)
  return self:_INTERNAL_Split_LoopGen(StoreAPI, StoreAPI, for_gpu)
end


function Split_Split_Traversal:_INTERNAL_Construct_Functions(L_API, R_API)
  local CACHE   = self:_INTERNAL_get_CACHE(L_API)
  if CACHE.FUNCTIONS_BUILT then return end

  self._left:_INTERNAL_Construct_Functions(  L_API )
  self._right:_INTERNAL_Construct_Functions( R_API )
  
  local function Split_Split_CPU_loopgen( storeptr, is_self_join,
                                          idxptr0, idxptr1,
                                          row0sym, row1sym, args,
                                          bodycode )
    local L_A_API   = self._left:_INTERNAL_A_API(L_API)
    local L_B_API   = self._left:_INTERNAL_B_API(L_API)
    local R_A_API   = self._right:_INTERNAL_A_API(R_API)
    local R_B_API   = self._right:_INTERNAL_B_API(R_API)

    local F         = false
    local aaloopgen = self._trav_AA:_INTERNAL_Split_LoopGen(L_A_API,R_A_API,F)
    local abloopgen = self._trav_AB:_INTERNAL_Split_LoopGen(L_A_API,R_B_API,F)
    local baloopgen = self._trav_BA:_INTERNAL_Split_LoopGen(L_B_API,R_A_API,F)
    local bbloopgen = self._trav_BB:_INTERNAL_Split_LoopGen(L_B_API,R_B_API,F)

    assert(terralib.issymbol(row0sym) and terralib.issymbol(row1sym),
            'INTERNAL: expected symbols')
    local r0sym     = symbol(row0sym.type, 'r0')
    local r1sym     = symbol(row1sym.type, 'r0')

    local loops     = newlist {
      aaloopgen( storeptr, is_self_join,
                 `&([idxptr0]._subidx_A), `&([idxptr1]._subidx_A),
                 row0sym, row1sym, args, bodycode )
    }
    -- only use the AB, not BA corner if we need a self-join
    if is_self_join then
      loops:insert( abloopgen( storeptr, false,
                      `[idxptr0]._subidx_A, `[idxptr1]._subidx_B,
                      r0sym, r1sym, args, quote
                        var [row0sym]   = r0sym
                        var [row1sym]   = r1sym
                        if row0sym > row1sym then
                          row0sym, row1sym = row1sym, row0sym
                        end
                        [bodycode]
                      end ) )
    else
      loops:insertall {
        abloopgen( storeptr, false,
                   `&([idxptr0]._subidx_A), `&([idxptr1]._subidx_B),
                   row0sym, row1sym, args, bodycode ),
        baloopgen( storeptr, false,
                   `&([idxptr0]._subidx_B), `&([idxptr1]._subidx_A),
                   row0sym, row1sym, args, bodycode ),
      }
    end
    loops:insert(
      bbloopgen( storeptr, is_self_join,
                 `&([idxptr0]._subidx_B), `&([idxptr1]._subidx_B),
                 row0sym, row1sym, args, bodycode )
    )

    return quote [loops] end
  end
  CACHE.Split_Split_CPU_loopgen = Split_Split_CPU_loopgen

  CACHE.FUNCTIONS_BUILT       = true
end




-------------------------------------------------------------------------------
--[[                                Exports                                ]]--
-------------------------------------------------------------------------------

Exports.is_split_split_traversal  = is_split_split_traversal
Exports.split_split_traversal     = new_split_split_traversal

Exports.is_split_index            = is_split_index
Exports.split_index               = new_split_index
