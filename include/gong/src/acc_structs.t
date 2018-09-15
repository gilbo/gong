--import 'gong.src.adt'

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
local is_function   = Functions.is_function

local newlist       = terralib.newlist

-------------------------------------------------------------------------------
--[[                           Helper Functions                            ]]--
-------------------------------------------------------------------------------

local minf = macro(function(a,b)
  return quote
    var r = [a]
    var t = [b]
  in terralib.select(t < r, t, r) end
end)
local maxf = macro(function(a,b)
  return quote
    var r = [a]
    var t = [b]
  in terralib.select(t > r, t, r) end
end)

local struct BBox {
  lo  : float[3]
  hi  : float[3]
}

-------------------------------------------------------------------------------
--[[                    Spatial Index Structure Objects                    ]]--
-------------------------------------------------------------------------------

local SpatialIndex    = {}
SpatialIndex.__index  = SpatialIndex

local function is_spatial_index(obj)
  return getmetatable(getmetatable(obj)) == SpatialIndex
end
Exports.is_spatial_index = is_spatial_index

function SpatialIndex:table() return self._table end



-- effectively no indexing
local Scan_Index      = setmetatable({},SpatialIndex)
Scan_Index.__index    = Scan_Index

function Scan_Index:__newindex(key, val)
  error("Cannot assign members to Scan_Index object",2)
end

function is_scan_index(obj)
  return getmetatable(obj) == Scan_Index
end
Exports.is_scan_index = is_scan_index

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

  local scanidx = setmetatable({
    _table          = args.table,
  }, Scan_Index)

  scan_index_cache[args.table] = scanidx
  return scanidx
end



local BVH_Index       = setmetatable({},SpatialIndex)
BVH_Index.__index     = BVH_Index

function BVH_Index:__newindex(key, val)
  error("Cannot assign members to BVH_Index object",2)
end

function is_bvh_index(obj)
  return getmetatable(obj) == BVH_Index
end
Exports.is_bvh_index  = is_bvh_index

local bvh_arg_msg = [[
new_bvh_index expects named arguments
{
  table       = gong data table to index
  volume      = gong record type for bounding volume
  abstract    = gong function : table -> volume
  vol_union   = gong function : { volume, volume } -> volume
  LEAF_SIZE   = max number of elements per leaf (optional)

  point       = gong function : volume -> vec3f
                (used to help build the BVH)
}
]]
local function new_bvh_index(args)
  if not args.table or
     not args.volume or
     not args.abstract or
     not args.vol_union or
     not args.point
  then
    error(bvh_arg_msg, 2)
  end
  if not is_table(args.table) then
    error("expected 'table' to be a gong data table\n"..bvh_arg_msg, 2)
  end
  if not is_type(args.volume) or not args.volume:is_record() then
    error("expected 'volume' to be a gong record type\n"..bvh_arg_msg, 2)
  end
  if not is_function(args.abstract) or
     #args.abstract:argtypes() ~= 1 or
     args.abstract:argtypes()[1] ~= T.row(args.table) or
     args.abstract:rettype() ~= args.volume
  then
    error("expected 'abstract' to be a gong function of type\n"..
          "  table -> volume\n"..bvh_arg_msg, 2)
  end
  if not is_function(args.vol_union) or
     #args.vol_union:argtypes() ~= 2 or
     args.vol_union:argtypes()[1] ~= args.volume or
     args.vol_union:argtypes()[2] ~= args.volume or
     args.vol_union:rettype() ~= args.volume
  then
    error("expected 'vol_union' to be a gong function of type\n"..
          "  { volume, volume } -> volume\n"..bvh_arg_msg, 2)
  end
  if args.LEAF_SIZE and not is_pos_int(args.LEAF_SIZE) then
    error("expected 'LEAF_SIZE' to be a positive integer\n", 2)
  end

  if not is_function(args.point) or
     #args.point:argtypes() ~= 1 or
     args.point:argtypes()[1] ~= args.volume or
     args.point:rettype() ~= T.vec3f
  then
    error("expected 'vol_union' to be a gong function of type\n"..
          "  { volume, volume } -> volume\n"..bvh_arg_msg, 2)
  end

  local bvhidx = setmetatable({
    _table          = args.table,
    _volume         = args.volume,
    _abstract       = args.abstract,
    _vol_union      = args.vol_union,
    _LEAF_SIZE      = args.LEAF_SIZE or 8,

    _point          = args.point,

    _ROOT           = 0,
    _CACHE          = {},
  }, BVH_Index)

  return bvhidx
end

Exports.scan_index          = new_scan_index
Exports.bvh_index           = new_bvh_index


-------------------------------------------------------------------------------
--[[                           Traversal Objects                           ]]--
-------------------------------------------------------------------------------

local Traversal       = {}
Traversal.__index     = Traversal

local function is_traversal(obj)
  return getmetatable(getmetatable(obj)) == Traversal
end
Exports.is_traversal  = is_traversal

function Traversal:for_cpu()  return self._cpu_enabled  end
function Traversal:for_gpu()  return self._gpu_enabled  end



local Scan_Scan_Traversal     = setmetatable({},Traversal)
Scan_Scan_Traversal.__index   = Scan_Scan_Traversal

function Scan_Scan_Traversal:__newindex(key, val)
  error("Cannot assign members to Scan_Scan_Traversal object",2)
end

local scanscan_arg_msg = [[
new_scan_scan_traversal() expects named arguments
{
  left        = gong spatial index to traverse
  right       = gong spatial index to traverse
}
]]
local function new_scan_scan_traversal(args)
  if not args.left or
     not args.right
  then
    error(scanscan_arg_msg, 2)
  end
  if not is_scan_index(args.left) or not is_scan_index(args.right) then
    error("expected 'left' and 'right' to be Scan_Index objects\n"..
          scanscan_arg_msg, 2)
  end

  local scanscan_trav = setmetatable({
    _left           = args.left,
    _right          = args.right,
    _cpu_enabled    = true,
    _gpu_enabled    = true,
    _CACHE          = {},
  }, Scan_Scan_Traversal)

  return scanscan_trav
end



local BVH_BVH_Traversal       = setmetatable({},Traversal)
BVH_BVH_Traversal.__index     = BVH_BVH_Traversal

function BVH_BVH_Traversal:__newindex(key, val)
  error("Cannot assign members to BVH_BVH_Traversal object",2)
end

local bvhbvh_arg_msg = [[
new_bvh_bvh_traversal() expects named arguments
{
  left        = gong spatial index to traverse
  right       = gong spatial index to traverse
  vol_isct    = gong function : { left.volume, right.volume } -> bool
}
]]
local function new_bvh_bvh_traversal(args)
  if not args.left or
     not args.right or
     not args.vol_isct
  then
    error(bvhbvh_arg_msg, 2)
  end
  if not is_bvh_index(args.left) or not is_bvh_index(args.right) then
    error("expected 'left' and 'right' to be BVH_Index objects\n"..
          bvhbvh_arg_msg, 2)
  end
  if not is_function(args.vol_isct) or
     #args.vol_isct:argtypes() ~= 2 or
     args.vol_isct:argtypes()[1] ~= args.left._volume or
     args.vol_isct:argtypes()[2] ~= args.right._volume or
     args.vol_isct:rettype() ~= T.bool
  then
    error("expected 'vol_isct' to be a gong function of type\n"..
          "  { left.volume, right.volume } -> bool\n"..bvh_arg_msg, 2)
  end

  local bvhbvh_trav = setmetatable({
    _left           = args.left,
    _right          = args.right,
    _cpu_enabled    = true,
    _gpu_enabled    = false,
    _CACHE          = {},
  }, BVH_BVH_Traversal)

  return bvhbvh_trav
end


Exports.scan_scan_traversal = new_scan_scan_traversal
Exports.bvh_bvh_traversal   = new_bvh_bvh_traversal


-------------------------------------------------------------------------------
--[[                                  Scan                                 ]]--
-------------------------------------------------------------------------------


function Scan_Index:_INTERNAL_StructLayout()
  if self._CACHE.SCAN then return self._CACHE.SCAN end

  local struct SCAN {}
  terra SCAN:init() end
  terra SCAN:destroy() end

  self._CACHE.SCAN = true
end

function Scan_Index:_INTERNAL_Construct_Functions()
  if self._CACHE.FUNCTIONS_BUILT then return end

  self._CACHE.FUNCTIONS_BUILT = true
end

function Scan_Scan_Traversal:_INTERNAL_LoopGen(StoreAPI, for_gpu)
  self:_INTERNAL_Construct_Functions(StoreAPI)

  if for_gpu then
    return self._CACHE.Scan_Scan_GPU_loopgen
  else
    return self._CACHE.Scan_Scan_CPU_loopgen
  end
end

function Scan_Scan_Traversal:_INTERNAL_Construct_Functions(StoreAPI)
  if self._CACHE.FUNCTIONS_BUILT then return end
  
  local function Scan_Scan_CPU_loopgen(storeptr, idxptr0, idxptr1,
                                                 row0sym, row1sym, bodycode)
    local tbl0type = T.row(self._left._table)
    local tbl1type = T.row(self._right._table)
    if tbl0type == tbl1type then
      return StoreAPI:SelfScan( storeptr, tbl0type,
                                          row0sym, row1sym, bodycode )
    else
      return StoreAPI:Scan( storeptr, tbl0type, row0sym,
                StoreAPI:Scan( storeptr, tbl1type, row1sym,
                  bodycode
                ))
    end
  end
  self._CACHE.Scan_Scan_CPU_loopgen = Scan_Scan_CPU_loopgen

  local function Scan_Scan_GPU_loopgen( name, storeptr,
                                        gpu_tblptr, gpu_globptr,
                                        idxptr0, idxptr1,
                                        row0sym, row1sym, args, bodycode)
    local tbl0type = T.row(self._left._table)
    local tbl1type = T.row(self._right._table)
    if tbl0type == tbl1type then
      return StoreAPI:GPU_SelfScan( name, storeptr,
                                    gpu_tblptr, gpu_globptr,
                                    tbl0type, row0sym, row1sym,
                                    args, bodycode )
    else
      return StoreAPI:GPU_DoubleScan( name, storeptr,
                                      gpu_tblptr, gpu_globptr,
                                      tbl0type, row0sym, tbl1type, row1sym,
                                      args, bodycode )
    end
  end
  self._CACHE.Scan_Scan_GPU_loopgen = Scan_Scan_GPU_loopgen

  self._CACHE.FUNCTIONS_BUILT = true
end


-------------------------------------------------------------------------------
--[[                                  BVH                                  ]]--
-------------------------------------------------------------------------------


--[[

Some way to construct the actual data structure layout
initialization
destruction

Some way to BUILD the data structure

Some way to UPDATE the data structure ??? degenerate to re-building?

Some way to traverse the data structure relative to another data structure

  -- what data does the structure depend on.  This helps control
  -- the question of whether to run the update?

  -- what functions the structure might invoke on data.
  -- This helps control the question of pre-processing?

--]]


function BVH_Index:_INTERNAL_StructLayout()
  if self._CACHE.BVH then return self._CACHE.BVH end

  local key_t         = T.row(self._table):terratype()
  local vol_t         = self._volume:terratype()

  assert(not self._table._is_live, 'INTERNAL: not supporting creating '..
         'a spatial index on a mergable table')
  assert(key_t == uint32, 'INTERNAL: expect table keys to be size uint32')

  local struct BVH_node {
    left    : key_t
    right   : key_t
    vol     : vol_t
  }
  terra BVH_node:is_leaf() : bool
    return [int32](self.left) < 0
  end
  terra BVH_node:get_leaf_contents() : {key_t,key_t}
    var leaf_start_idx  = [uint32]( -[int32](self.left) )
    var leaf_stop_idx   = self.right
    return leaf_start_idx, leaf_stop_idx
  end
  terra BVH_node:set_leaf_contents(start : key_t, stop : key_t)
    self.left           = [uint32]( -[int32](start) )
    self.right          = stop
  end

  local struct BVH {
    bvh_nodes   : vector(BVH_node)
    geom_ids    : vector(key_t)
    geom_vols   : vector(VolTyp)
    n_updates   : uint32    -- track how many updates without a rebuild
  }
  terra BVH:init()
    self.bvh_nodes:init()
    self.geom_ids:init()
    self.geom_vols:init()
    self.n_updates = 0
  end
  terra BVH:destroy()
    self.bvh_nodes:destroy()
    self.geom_ids:destroy()
    self.geom_vols:destroy()
  end

  self._CACHE.BVH_node      = BVH_node
  self._CACHE.BVH           = BVH

  return BVH
end


function BVH_Index:_INTERNAL_Build(StoreAPI)
  self:_INTERNAL_Construct_Functions(StoreAPI)

  return self._CACHE.Build
end

function BVH_Index:_INTERNAL_Update(StoreAPI)
  self:_INTERNAL_Construct_Functions(StoreAPI)

  return self._CACHE.Update
end

function BVH_Index:_INTERNAL_Construct_Functions(StoreAPI)
  if self._CACHE.FUNCTIONS_BUILT then return end

  local key_t         = T.row(self._table):terratype()
  local vol_t         = self._volume:terratype()

  local BVH                 = self._CACHE.BVH
  local pointfn             = StoreAPI:GetCPUFunction( self.point )
  local absfn               = StoreAPI:GetCPUFunction( self.abstract )
  local unionfn             = StoreAPI:GetCPUFunction( self.vol_union )

  local point = macro(function(storeptr, bvh, k)
    return `pointfn(storeptr, bvh.geom_vols[k]).d[dim]
  end)

  terra BVH:quick_select( storeptr : &(StoreAPI.StoreTyp), mid : key_t,
                          start : key_t, stop : key_t, dim : uint)
    -- termination condition
    if stop - 1 == mid then return end

    -- pivot index ; pivot value   pi ; pv
    var p_i : key_t = ( C.rand() % (stop-start) ) + start
    var p_v         = point(storeptr, self, p_i)[dim]

    var front       = start
    var back        = stop-1
    while front < back do -- run through and copy
      while point(storeptr, self, front)[dim] < p_v do front = front + 1 end
      while point(storeptr, self, back)[dim]  > p_v do back  = back  - 1 end
      -- now we have a pair to swap...
      self.geom_ids[front], self.geom_ids[back] =
      self.geom_ids[back], self.geom_ids[front]
      self.geom_vols[front], self.geom_vols[back] =
      self.geom_vols[back], self.geom_vols[front]
      front = front + 1
      back  = back  - 1
    end
    if front == back and point(storeptr,self,front)[dim] <= p_v then
      front = front + 1
    end

    if mid < front then
      self:quick_select(storeptr, mid, start, front, dim)
    else
      self:quick_select(storeptr, mid, front, stop, dim)
    end
  end

  terra BVH:tree_build( storeptr : &(StoreAPI.StoreTyp),
                        start : key_t, stop : key_t, last_dim : uint )
    var node_i      = self.bvh_nodes:size()
                      self.bvh_nodes:resize(node_i+1)
    -- base case
    if stop - start <= LEAF_SIZE then
      -- compute the bounding volume of the leaf
      assert(stop-start > 0, 'INTERNAL: expect non-empty leaves')
      var vol       = self.geom_vols[start]
      for k=start+1,stop do
        vol = unionfn(storeptr, vol, self.geom_vols[k])
      end
      self.bvh_nodes[node_i]:set_leaf_contents(start, stop)
      self.bvh_nodes[node_i].vol  = vol
    else
      -- shuffle the backing array
      var dim       = (last_dim+1) % 3
      var mid       = (start+stop) / 2
      self:quick_select(storeptr, mid, start, stop, dim)

      -- recurse, and then build this node
      var left      = self:tree_build(storeptr, start, mid, dim)
      var right     = self:tree_build(storeptr,  mid, stop, dim)
      self.bvh_nodes[node_i].left   = left
      self.bvh_nodes[node_i].right  = right
      var lvol      = self.bvh_nodes[left].vol
      var rvol      = self.bvh_nodes[right].vol
      self.bvh_nodes[node_i].vol    = unionfn(storeptr, lvol, rvol)
    end
    -- regardless, we built node number...
    return node_i
  end

  -- construct tree from data set
  -- safe for rebuilding with
  terra BVH:build( s : &(StoreAPI.StoreTyp) )
    var this    = self
    var k       : key_t
    var SIZE    = [ StoreAPI:Size( s, T.row(Table) ) ]

    -- extract the bounding volumes to begin with
    this.geom_ids:resize(SIZE)
    this.geom_vols:resize(SIZE)
    this.bvh_nodes:resize(0)
    [ StoreAPI:Scan( s, T.row(Table), k, quote
        this.geom_ids[k]    = k
        this.geom_vols[k]   = absfn(s,k)
    end ) ]

    -- recursively build the tree
    if SIZE > 0 then
      assert(ROOT == self:tree_build( s, 0, SIZE, 2 ), 'INTERNAL: bad root')
    end
  end

  terra BVH:update_helper( storeptr : &(StoreAPI.StoreTyp), node_i : key_t )
    var node                      = self.bvh_nodes[node_i]
    if node:is_leaf() then
      var start, stop             = node:get_leaf_contents()
      assert(stop > start, 'INTERNAL: bad leaf range')
      var gi                      = self.geom_ids[start]
      var vol : VolTyp            = absfn(storeptr,gi)
      self.geom_vols[start]       = vol
      for k=start+1,stop do
        var gi                    = self.geom_ids[k]
        var v                     = absfn(storeptr,gi)
        vol                       = unionfn(storeptr, vol, v)
        self.geom_vols[k]         = v
      end
      self.bvh_nodes[node_i].vol  = vol
      return vol
    else
      var li                      = self.bvh_nodes[node_i].left
      var ri                      = self.bvh_nodes[node_i].right
      var lvol                    = self:refit_helper(storeptr,li)
      var rvol                    = self:refit_helper(storeptr,ri)
      var vol                     = unionfn(storeptr, lvol, rvol)
      self.bvh_nodes[node_i].vol  = vol
      return vol
    end
  end
  terra BVH:update( storeptr : &(StoreAPI.StoreTyp) )
    if self.geom_ids:size() > 0 then
      if self.n_updates > 7 then
        BVH:refit_helper(storeptr, ROOT)
        self.n_updates = 0
      else
        self.n_updates = self.n_updates + 1
      end
    end
  end

  self._CACHE.FUNCTIONS_BUILT     = true
end


function BVH_BVH_Traversal:_INTERNAL_LoopGen(StoreAPI, for_gpu)
  self:_INTERNAL_Construct_Functions(StoreAPI)
  assert(not for_gpu, 'INTERNAL: expect BVH to be CPU only')
  return self._CACHE.BVH_BVH_loopgen
end

function BVH_BVH_Traversal:_INTERNAL_Construct_Functions(StoreAPI)
  if self._CACHE.FUNCTIONS_BUILT then return end

  local BVH_L               = self._left._CACHE.BVH
  local BVH_R               = self._right._CACHE.BVH
  local key_t_L             = T.row(self._left._table):terratype()
  local key_t_R             = T.row(self._right._table):terratype()
  local vol_t_L             = self._left._volume:terratype()
  local vol_t_R             = self._right._volume:terratype()

  local struct WorkPacket {
    i_L       : key_t_L
    i_R       : key_t_R
  }

  local isctfn              = StoreAPI:GetCPUFunction( self.vol_isct )

  local function BVH_BVH_loopgen(storeptr, idxptr0, idxptr1,
                                           row0sym, row1sym, bodycode)
    assert(terralib.issymbol(storeptr), 'INTERNAL: expect symbol')
    local StorePtrType  = storeptr.type

    local terra bvh_loop( s : StorePtrType, bvh_L : &BVH_L, bvh_R : &BVH_R )
      -- exit early if either operand table is empty
      if bvh_L.geom_ids:size() == 0 or bvh_R.geom_ids:size() == 0 then
        return
      end

      var i_L : key_t_L     = self._left._ROOT
      var i_R : key_t_R     = self._right._ROOT
      var work_stack : stack(WorkPacket)
      work_stack:init()

      var node_L  = bvh_L.bvh_nodes[i_L]
      var node_R  = bvh_R.bvh_nodes[i_R]
      var vol_L   = node_L.vol
      var vol_R   = node_R.vol

      var flip_dir = true

      repeat
        var do_pop  = true
        if isctfn(s, vol_L, vol_R) then
          var leaf_L    = vol_L:is_leaf()
          var leaf_R    = vol_R:is_leaf()
          if leaf_L and leaf_R then
            var start_L, stop_L = node_L:get_leaf_contents()
            var start_R, stop_R = node_R:get_leaf_contents()
            for k_L = start_L,stop_L do
              vol_L             = bvh_L.geom_vols[k_L]
              var [row0sym]     = bvh_L.geom_ids[k_L]
              for k_R = start_R,stop_R do
                vol_R           = bvh_R.geom_vols[k_R]
                var [row1sym]   = bvh_R.geom_ids[k_R]
                if isctfn(s, vol_L, vol_R) then
                  var [storeptr]  = s
                  [bodycode]
                end
              end
            end
            -- INNER LOOP
          else
            do_pop = false
            var go_left : bool
            if      leaf_L then go_left = false
            elseif  leaf_R then go_left = true
            else
              flip_dir  = not flip_dir
              go_left   = flip_dir
            end
            if go_left then
              work_stack:push( WorkPacket{ i_L=node_L.right, i_R=i_R })
              i_L     = node_L.left
              node_L  = bvh_L.bvh_nodes[i_L]
              vol_L   = node_L.vol
            else
              work_stack:push( WorkPacket{ i_L=i_L, i_R=node_R.right })
              i_R     = node_R.left
              node_R  = bvh_R.bvh_nodes[i_R]
              vol_R   = node_R.vol
            end
          end
        end

        if do_pop then
          var packet  = work_stack:pop()
          i_L         = packet.i_L
          i_R         = packet.i_R
          node_L      = bvh_L.bvh_nodes[i_L]
          node_R      = bvh_R.bvh_nodes[i_R]
          vol_L       = node_L.vol
          vol_R       = node_R.vol
        end
      until work_stack:is_empty()

      work_stack:destroy()
    end

    return quote
      bvh_loop( [storeptr], [idxptr0], [idxptr1] )
    end
  end

  self._CACHE.BVH_BVH_loopgen = BVH_BVH_loopgen
  self._CACHE.FUNCTIONS_BUILT   = true
end







