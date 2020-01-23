
local Exports = {}
package.loaded["gong.src.acc_bvh"] = Exports

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

local GPU           = require 'gong.src.gpu_util'

local Schemata      = require 'gong.src.schemata'
local is_table      = Schemata.is_table

local Functions     = require 'gong.src.functions'

local AccStructs    = require 'gong.src.acc_structs'

local newlist       = terralib.newlist

-------------------------------------------------------------------------------
--[[                           Helper Functions                            ]]--
-------------------------------------------------------------------------------

local gmalloc = macro(function(typ,N)
  typ = assert(typ:astype())
  return `[&typ](GPU.malloc( sizeof(typ) * [N] ))
end)
local gfree = macro(function(p)
  return `GPU.free([p])
end)

-------------------------------------------------------------------------------
--[[                    Spatial Index Structure Objects                    ]]--
-------------------------------------------------------------------------------

local BVH_Index       = setmetatable({},AccStructs.SpatialIndex)
BVH_Index.__index     = BVH_Index

function BVH_Index:__newindex(key, val)
  error("Cannot assign members to BVH_Index object",2)
end

function is_bvh_index(obj)
  return getmetatable(obj) == BVH_Index
end

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
  if args.volume:has_rows() then
    error("expected 'volume' to have no references to table rows", 2)
  end
  if not Functions.is_function(args.abstract) or
     #args.abstract:argtypes() ~= 1 or
     args.abstract:argtypes()[1] ~= T.row(args.table) or
     args.abstract:rettype() ~= args.volume
  then
    error("expected 'abstract' to be a gong function of type\n"..
          "  table -> volume\n"..bvh_arg_msg, 2)
  end
  if not Functions.is_function(args.vol_union) or
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

  if not Functions.is_function(args.point) or
     #args.point:argtypes() ~= 1 or
     args.point:argtypes()[1] ~= args.volume or
     args.point:rettype() ~= T.vec3f
  then
    error("expected 'vol_union' to be a gong function of type\n"..
          "  { volume, volume } -> volume\n"..bvh_arg_msg, 2)
  end

  if #( args.vol_union:_INTERNAL_geteffects():filter(function(e)
          return e:touches_memory()
        end) ) > 0 then
    error("expected 'vol_union' to be a pure function", 2) end
  if #( args.point:_INTERNAL_geteffects():filter(function(e)
          return e:touches_memory()
        end) ) > 0 then
    error("expected 'point' to be a pure function", 2) end

  local bvhidx = AccStructs.NewSpatialIndexObj({
    _table          = args.table,
    _volume         = args.volume,
    _abstract       = args.abstract,
    _vol_union      = args.vol_union,
    _LEAF_SIZE      = args.LEAF_SIZE or 8,

    _point          = args.point,

    _ROOT           = 0,

    _subfuncs       = newlist{ args.abstract,
                               args.vol_union,
                               args.point, },
  }, BVH_Index)

  return bvhidx
end

function BVH_Index:name() return self._table:name().."_BVH_Index" end



-------------------------------------------------------------------------------
--[[                           Traversal Objects                           ]]--
-------------------------------------------------------------------------------

local BVH_BVH_Traversal       = setmetatable({},AccStructs.Traversal)
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
  if not Functions.is_function(args.vol_isct) or
     #args.vol_isct:argtypes() ~= 2 or
     args.vol_isct:argtypes()[1] ~= args.left._volume or
     args.vol_isct:argtypes()[2] ~= args.right._volume or
     args.vol_isct:rettype() ~= T.bool
  then
    error("expected 'vol_isct' to be a gong function of type\n"..
          "  { left.volume, right.volume } -> bool\n"..bvh_arg_msg, 2)
  end

  if #( args.vol_isct:_INTERNAL_geteffects():filter(function(e)
          return e:touches_memory()
        end) ) > 0 then
    error("expected 'vol_isct' to be a pure function", 2) end

  local bvhbvh_trav = AccStructs.NewTraversalObj({
    _left           = args.left,
    _right          = args.right,
    _gpu_scan       = 'left', -- default to left scan for GPU for now
    _cpu_enabled    = true,
    _gpu_enabled    = true,
    _vol_isct       = args.vol_isct,
    _subfuncs       = newlist{ args.vol_isct },
  }, BVH_BVH_Traversal)

  return bvhbvh_trav
end

local function is_bvh_bvh_traversal(obj)
  return getmetatable(obj) == BVH_BVH_Traversal
end




-------------------------------------------------------------------------------
--[[                           BVH Implementation                          ]]--
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


function BVH_Index:_INTERNAL_StructLayout(StoreAPI)
  local CACHE         = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.BVH then return CACHE.BVH end

  local key_t         = T.row(self._table):terratype()
  local vol_t         = self._volume:terratype()

  assert(not self._table._is_live, 'INTERNAL: not supporting creating '..
         'a spatial index on a mergable table')
  assert(key_t == uint32, 'INTERNAL: expect table keys to be size uint32')

  local HIGH_BIT      = constant(key_t, math.pow(2,31))
  local struct BVH_node {
    left    : key_t
    right   : key_t
    vol     : vol_t
  }
  terra BVH_node:is_leaf() : bool
    return (HIGH_BIT and self.left) ~= 0
  end
  terra BVH_node:get_leaf_contents() : {key_t,key_t}
    var leaf_start_idx  = self.left and not HIGH_BIT
    var leaf_stop_idx   = self.right
    return leaf_start_idx, leaf_stop_idx
  end
  terra BVH_node:set_leaf_contents(start : key_t, stop : key_t)
    self.left           = start or HIGH_BIT
    self.right          = stop
  end

  local struct BVH {
    bvh_nodes   : vector(BVH_node)
    geom_ids    : vector(key_t)
    geom_vols   : vector(vol_t)
    n_updates   : uint32    -- track how many updates without a rebuild
    rebuild     : bool
    refit       : bool
  }
  local gpu_on = StoreAPI:HasGPUSupport()
  if gpu_on then
    local struct GPU_BVH_node {
      left      : key_t
      right     : key_t
    }
    local struct GPU_BVH {
      n_geom    : uint32
      nodes     : &GPU_BVH_node
      --parents   : &key_t
      node_vols : &vol_t
      geom_vols : &vol_t
      ids       : &key_t
    }
    BVH.entries:insertall{
      {'cpu_data',GPU_BVH},
      {'gpu_data',&GPU_BVH},
      {'cpu_valid',bool},
      {'gpu_valid',bool},
      {'gpu_refit',bool},
      {'gpu_rebuild',bool},
    }


    terra GPU_BVH:init()
      self.n_geom     = 0
      self.nodes      = gmalloc(GPU_BVH_node, 1)
      --self.parents    = gmalloc(key_t, 1)
      self.node_vols  = gmalloc(vol_t, 1)
      self.geom_vols  = gmalloc(vol_t, 1)
      self.ids        = gmalloc(key_t, 1)
    end
    terra GPU_BVH:destroy()
      gfree(self.nodes)
      --gfree(self.parents)
      gfree(self.node_vols)
      gfree(self.geom_vols)
      gfree(self.ids)
    end
    terra BVH:bvh_meta_to_gpu()
      assert(self.cpu_valid, 'expected gpu_bvh valid on CPU')
      if self.gpu_valid then return end
      --MEMCPY to GPU
      GPU.memcpy_to_gpu(self.gpu_data, &(self.cpu_data), sizeof(GPU_BVH))
      self.gpu_valid = true
    end
    terra BVH:bvh_meta_to_cpu()
      assert(self.gpu_valid, 'expected gpu_bvh valid on GPU')
      if self.cpu_valid then return end
      --MEMCPY to CPU
      GPU.memcpy_from_gpu(&(self.cpu_data), self.gpu_data, sizeof(GPU_BVH))
      self.cpu_valid = true
    end

    BVH.metamethods.gpu_tag = true

    CACHE.GPU_BVH_node  = GPU_BVH_node
    CACHE.GPU_BVH       = GPU_BVH
  end
  terra BVH:init()
    self.bvh_nodes:init()
    self.geom_ids:init()
    self.geom_vols:init()
    self.n_updates  = 0
    self.rebuild    = true
    self.refit      = true
    escape if gpu_on then emit quote
      self.cpu_data:init()
      self.gpu_data   = gmalloc(CACHE.GPU_BVH, 1)
      self.cpu_valid  = true
      self.gpu_valid  = false
      self:bvh_meta_to_gpu()
      self.gpu_refit    = true
      self.gpu_rebuild  = true
    end end end
  end
  terra BVH:destroy()
    escape if gpu_on then emit quote
      self.cpu_data:destroy()
      gfree(self.gpu_data)
      self.cpu_valid  = false
      self.gpu_valid  = false
    end end end
    self.bvh_nodes:destroy()
    self.geom_ids:destroy()
    self.geom_vols:destroy()
  end

  CACHE.BVH_node    = BVH_node
  CACHE.BVH         = BVH

  return BVH
end

function BVH_Index:_INTERNAL_do_invalidate(idxptr, size_invalid,data_invalid)
  local IDX_TYPE  = idxptr:gettype()
  if IDX_TYPE:ispointer() then IDX_TYPE = IDX_TYPE.type end
  local last_entry
  local use_gpu   = IDX_TYPE.metamethods.gpu_tag

  return quote
    idxptr.rebuild  = idxptr.rebuild  or size_invalid
    idxptr.refit    = idxptr.refit    or data_invalid
    escape if use_gpu then emit quote
      idxptr.gpu_rebuild = idxptr.gpu_rebuild or size_invalid
      idxptr.gpu_refit   = idxptr.gpu_refit   or data_invalid
    end end end
    --idxptr.rebuild  = true -- hack for now...
  end
end

function BVH_Index:_INTERNAL_PreJoinUpdate( StoreAPI,
                                            name, storeptr, idxptr,
                                            gpu_tblptr, gpu_globptr )
  self:_INTERNAL_Construct_Functions(StoreAPI)
  if gpu_tblptr then
    self:_INTERNAL_Construct_GPU_Functions(StoreAPI)
  end

  return quote end
end

function BVH_Index:_INTERNAL_Construct_Functions(StoreAPI)
  local CACHE         = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.FUNCTIONS_BUILT then return end

  local ROOT          = self._ROOT
  local RowTyp        = T.row(self._table)
  local key_t         = T.row(self._table):terratype()
  local vol_t         = self._volume:terratype()

  local BVH                 = CACHE.BVH
  local LEAF_SIZE           = self._LEAF_SIZE
  local pointfn             = StoreAPI:GetCPUFunction( self._point )
  local absfn               = StoreAPI:GetCPUFunction( self._abstract )
  local unionfn             = StoreAPI:GetCPUFunction( self._vol_union )

  local point = macro(function(storeptr, bvh, k)
    return `pointfn(storeptr, bvh.geom_vols(k))
  end)

  local StorePtr            = &(StoreAPI:StoreTyp())

  terra BVH:quick_select( storeptr : StorePtr, mid : key_t,
                          start : key_t, stop : key_t, dim : uint) : {}
    -- termination condition
    if stop - 1 == mid then return end

    -- pivot index ; pivot value   pi ; pv
    var p_i : key_t = ( C.rand() % (stop-start) ) + start
    var p_v         = point(storeptr, self, p_i).d[dim]

    var front       = start
    var back        = stop-1
    while front < back do -- run through and copy
      while front < back and point(storeptr, self, front).d[dim] < p_v do
        front = front + 1
      end
      while front < back and point(storeptr, self, back).d[dim]  > p_v do
        back  = back  - 1
      end
      if front < back then
        -- now we have a pair to swap...
        self.geom_ids(front), self.geom_ids(back) =
        self.geom_ids(back), self.geom_ids(front)
        self.geom_vols(front), self.geom_vols(back) =
        self.geom_vols(back), self.geom_vols(front)
        front = front + 1
        back  = back  - 1
      end
    end
    if front == back and point(storeptr,self,front).d[dim] <= p_v then
      front = front + 1
    end

    if mid < front then
      self:quick_select(storeptr, mid, start, front, dim)
    else
      self:quick_select(storeptr, mid, front, stop, dim)
    end
  end

  terra BVH:tree_build( storeptr : StorePtr,
                        start : key_t, stop : key_t, last_dim : uint ) : key_t
    var node_i      = self.bvh_nodes:size()
                      self.bvh_nodes:resize(node_i+1)
    -- base case
    if stop - start <= LEAF_SIZE then
      --C.printf("LEAF %4d: %4d %4d\n", node_i, start, stop)
      -- compute the bounding volume of the leaf
      assert(stop-start > 0, 'INTERNAL: expect non-empty leaves')
      var vol       = self.geom_vols(start)
      for k=start+1,stop do
        vol = unionfn(storeptr, vol, self.geom_vols(k))
      end
      self.bvh_nodes(node_i):set_leaf_contents(start, stop)
      self.bvh_nodes(node_i).vol  = vol
    else
      -- shuffle the backing array
      var dim       = (last_dim+1) % 3
      var mid       = (start+stop) / 2
      --C.printf("    - %4d (%d,%d,%d;%d)\n", node_i, mid, start, stop, dim)
      self:quick_select(storeptr, mid, start, stop, dim)

      -- recurse, and then build this node
      var left      = self:tree_build(storeptr, start, mid, dim)
      var right     = self:tree_build(storeptr,  mid, stop, dim)
      --C.printf("NODE %4d: %4d %4d\n", node_i, left, right)
      self.bvh_nodes(node_i).left   = left
      self.bvh_nodes(node_i).right  = right
      var lvol      = self.bvh_nodes(left).vol
      var rvol      = self.bvh_nodes(right).vol
      self.bvh_nodes(node_i).vol    = unionfn(storeptr, lvol, rvol)
    end
    -- regardless, we built node number...
    return node_i
  end

  -- construct tree from data set
  -- safe for rebuilding with
  terra BVH:build( s : StorePtr )
    --C.printf("BUILD BVH\n")
    var this    = self
    var k       : key_t
    var SIZE    = [ StoreAPI:Size( s, RowTyp ) ]

    -- extract the bounding volumes to begin with
    this.geom_ids:resize(SIZE)
    this.geom_vols:resize(SIZE)
    this.bvh_nodes:resize(0)
    var w       = [key_t](0)
    [ StoreAPI:Scan( s, RowTyp, k, quote
        this.geom_ids(w)    = k
        this.geom_vols(w)   = absfn(s,k)
        w                   = w + 1
    end ) ]

    -- recursively build the tree
    if SIZE > 0 then
      assert(ROOT == self:tree_build( s, 0, SIZE, 2 ), 'INTERNAL: bad root')
    end

    self.rebuild    = false
    self.refit      = false
    self.n_updates  = 0
  end

  -- this helper re-fits volumes of the existing tree at node_i, recursively
  terra BVH:update_helper( storeptr : StorePtr, node_i : key_t ) : vol_t
    var node                      = self.bvh_nodes(node_i)
    if node:is_leaf() then
      var start, stop             = node:get_leaf_contents()
      assert(stop > start, 'INTERNAL: bad leaf range')
      var gi                      = self.geom_ids(start)
      var vol : vol_t             = absfn(storeptr,gi)
      self.geom_vols(start)       = vol
      for k=start+1,stop do
        var gi                    = self.geom_ids(k)
        var v                     = absfn(storeptr,gi)
        vol                       = unionfn(storeptr, vol, v)
        self.geom_vols(k)         = v
      end
      self.bvh_nodes(node_i).vol  = vol
      return vol
    else
      var li                      = self.bvh_nodes(node_i).left
      var ri                      = self.bvh_nodes(node_i).right
      var lvol                    = self:update_helper(storeptr,li)
      var rvol                    = self:update_helper(storeptr,ri)
      var vol                     = unionfn(storeptr, lvol, rvol)
      self.bvh_nodes(node_i).vol  = vol
      return vol
    end
  end
  terra BVH:update( storeptr : StorePtr )
    --C.printf('** in update: rebuild/refit (%d/%d) n_updates: %d\n',
    --         self.rebuild, self.refit, self.n_updates)
    if self.rebuild then
      self:build(storeptr)
    elseif self.refit then
      if self.geom_ids:size() > 0 then
        if self.n_updates < 7 then
          self:update_helper(storeptr, ROOT)
          self.n_updates  = self.n_updates + 1
        else
          self:build(storeptr)
        end
      end
      self.refit  = false
    end
  end

  CACHE.FUNCTIONS_BUILT     = true
end

function BVH_Index:_INTERNAL_Construct_GPU_Functions(StoreAPI)
  local CACHE         = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.GPU_FUNCTIONS_BUILT then return end

  local ROOT          = self._ROOT
  local RowTyp        = T.row(self._table)
  local key_t         = RowTyp:terratype()
  local vol_t         = self._volume:terratype()

  local name          = self:name()

  local BVH                 = CACHE.BVH
  local GPU_BVH             = CACHE.GPU_BVH
  local GPU_BVH_node        = CACHE.GPU_BVH_node
  local LEAF_SIZE           = self._LEAF_SIZE
  local pointfn             = StoreAPI:GetGPUFunction( self._point )
  local absfn               = StoreAPI:GetGPUFunction( self._abstract )
  local unionfn             = StoreAPI:GetGPUFunction( self._vol_union )

  local vec3f_t             = T.vec3f:terratype()

  local StorePtr            = &(StoreAPI:StoreTyp())
  local GTbl                = StoreAPI:GPU_TablesTyp()
  local GGlob               = StoreAPI:GPU_GlobalsTyp()
  local tblptr              = symbol(&GTbl, 'gpu_tables')
  local globptr             = symbol(&GGlob, 'gpu_tables')


  --  local struct GPU_BVH_node {
  --    left      : key_t
  --    right     : key_t
  --  }
  --  local struct GPU_BVH {
  --    n_geom    : uint32
  --    nodes     : &GPU_BVH_node
  --    --parents   : &key_t
  --    node_vols : &vol_t
  --    geom_vols : &vol_t
  --    ids       : &key_t
  --  }
  --  BVH.entries:insertall{
  --    {'cpu_data',GPU_BVH},
  --    {'gpu_data',&GPU_BVH},
  --    {'cpu_valid',bool},
  --    {'gpu_valid',bool},
  --    {'gpu_refit',bool},
  --    {'gpu_rebuild',bool},
  --  }

  terra BVH:gpu_debug_print(max : uint32)
    if not self.cpu_valid then self:bvh_meta_to_cpu() end
    var this      = self
    var N         = this.cpu_data.n_geom
    if max == 0 then max = N end
    var nodes     = [&GPU_BVH_node](C.malloc( sizeof(GPU_BVH_node) * N ))
    var node_vols = [&vol_t](C.malloc( sizeof(vol_t) * N ))
    var geom_vols = [&vol_t](C.malloc( sizeof(vol_t) * N ))
    var ids       = [&key_t](C.malloc( sizeof(key_t) * N ))
    GPU.memcpy_from_gpu( nodes, this.cpu_data.nodes, N*sizeof(GPU_BVH_node) )
    GPU.memcpy_from_gpu( node_vols, this.cpu_data.node_vols, N*sizeof(vol_t) )
    GPU.memcpy_from_gpu( geom_vols, this.cpu_data.geom_vols, N*sizeof(vol_t) )
    GPU.memcpy_from_gpu( ids, this.cpu_data.ids, N*sizeof(key_t) )

    C.printf("BVH GPU size #%d\n", N)
    C.printf("Nodes\n")
    for k=0,max do
      C.printf("%6d:  %6d %6d\n", k, nodes[k].left, nodes[k].right)
      var vol = node_vols[k]
      C.printf(
        "                       %7.3f %7.3f %7.3f , %7.3f %7.3f %7.3f\n",
               vol.lo.d[0], vol.lo.d[1], vol.lo.d[2],
               vol.hi.d[0], vol.hi.d[1], vol.hi.d[2])
    end

    C.printf("Leaves\n")
    for k=0,max do
      var vol = geom_vols[k]
      C.printf("%6d:->%6d   %7.3f %7.3f %7.3f , %7.3f %7.3f %7.3f\n",
               k, ids[k],
               vol.lo.d[0], vol.lo.d[1], vol.lo.d[2],
               vol.hi.d[0], vol.hi.d[1], vol.hi.d[2])
    end

    C.free(nodes)
    C.free(node_vols)
    C.free(geom_vols)
    C.free(ids)
  end

  terra BVH:gpu_fit_volumes( s : StorePtr )
    --C.printf("REFIT GPU BVH\n")
    var this      = self
    var id        : key_t
    var N         = this.cpu_data.n_geom
    var gpu_data  = this.gpu_data

    var visits    = gmalloc(uint32, N)
    GPU.memzero( visits, sizeof(uint32)*N )

    -- first, update the leaf node volumes
    [ StoreAPI:GPU_Scan( name..'_leaffit', s, tblptr, globptr,
                         RowTyp, id, newlist{ gpu_data },
      quote
        var N         = gpu_data.n_geom

        -- first, update the leaf node volume
        var row       = gpu_data.ids[id]
        var leaf_vol  = absfn(tblptr, globptr, row)
        gpu_data.geom_vols[id] = leaf_vol
      end) ]

    -- then, update the internal nodes
    for iter=1,63 do
      [ StoreAPI:GPU_Scan( name..'_volfit', s, tblptr, globptr,
                           RowTyp, id, newlist{ gpu_data, visits, iter },
        quote
          var N           = gpu_data.n_geom
          if id == N-1 then return end
          --var root_visit  = visits[0]
          ---- early exit on superfluous later iterations...
          --if root_visit > 0 and root_visit < iter then return end

          -- otherwise, see if this node can be updated...
          var i           = id
          var i_visit     = visits[i]
          -- exit if the node was already updated
          if i_visit > 0 and i_visit < iter then return end
          -- otherwise, see if both its children have been updated
          var l_i         = gpu_data.nodes[i].left
          var r_i         = gpu_data.nodes[i].right
          var l_ok, r_ok  = true, true
          if l_i < N then l_ok = (visits[l_i] > 0 and visits[l_i] < iter) end
          if r_i < N then r_ok = (visits[r_i] > 0 and visits[r_i] < iter) end
          -- if not, then we need to wait
          if not l_ok or not r_ok then return end

          -- both children have been updated, but not this node!
          -- so, do the update.
          var l_vol : vol_t
          var r_vol : vol_t
          if l_i < N then l_vol = gpu_data.node_vols[l_i]
                     else l_vol = gpu_data.geom_vols[l_i-N] end
          if r_i < N then r_vol = gpu_data.node_vols[r_i]
                     else r_vol = gpu_data.geom_vols[r_i-N] end
          var vol               = unionfn(tblptr, globptr, l_vol, r_vol)
          gpu_data.node_vols[i] = vol

          -- and mark this node as visited
          --GPU.printf("fitting visit to %6d on round %6d\n", i, iter)
          visits[i]       = iter
        end) ]
    end
    --this:gpu_debug_print(10)

    gfree(visits)
  end

  local INFTY = constant(float, math.huge)
  terra BVH:gpu_build( s : StorePtr )
    --C.printf("BUILD GPU BVH\n")
    var this      = self
    var row       : key_t
    var SIZE      = [ StoreAPI:Size( s, RowTyp ) ]
    var old_size  = this.cpu_data.n_geom

    -- ALLOC : space for codes
    var codes     = gmalloc(uint32, SIZE)
    var code_tmp  = gmalloc(uint32, SIZE)
    var id_tmp    = gmalloc(key_t, SIZE)
    var pts       = gmalloc(vec3f_t, SIZE)
    var lo_pt     = gmalloc(vec3f_t, 1)
    var hi_pt     = gmalloc(vec3f_t, 1)
    -- initialize pt bounds
    do
      var inf     = vec3f_t{ d=array(  INFTY,  INFTY,  INFTY ) }
      var ninf    = vec3f_t{ d=array( -INFTY, -INFTY, -INFTY ) }
      GPU.memcpy_to_gpu(lo_pt, &inf, sizeof(vec3f_t))
      GPU.memcpy_to_gpu(hi_pt, &ninf, sizeof(vec3f_t))
    end
    if SIZE ~= old_size then
      -- re-allocate on size changes
      this.cpu_data.n_geom    = SIZE
      gfree(this.cpu_data.nodes)
      --gfree(this.cpu_data.parents)
      gfree(this.cpu_data.node_vols)
      gfree(this.cpu_data.geom_vols)
      gfree(this.cpu_data.ids)
      this.cpu_data.nodes     = gmalloc(GPU_BVH_node, SIZE)
      --this.cpu_data.parents   = gmalloc(key_t, 2*SIZE)
      this.cpu_data.geom_vols = gmalloc(vol_t, SIZE)
      this.cpu_data.node_vols = gmalloc(vol_t, SIZE)
      this.cpu_data.ids       = gmalloc(key_t, SIZE)
      -- force refresh to GPU
      this.gpu_valid          = false
      this:bvh_meta_to_gpu()
    end
    var gpu_data = this.gpu_data

    -- Kernel 1 : compute volumes, Morton codes, and init id array
    [ StoreAPI:GPU_Scan( name..'_genpts', s, tblptr, globptr,
                         RowTyp, row, newlist{ pts, lo_pt, hi_pt },
      quote
        var vol       = absfn(tblptr, globptr, row)
        var pt        = pointfn(tblptr, globptr, vol)
        pts[row]      = pt
        GPU.reduce_min_float(&(lo_pt.d[0]), pt.d[0])
        GPU.reduce_min_float(&(lo_pt.d[1]), pt.d[1])
        GPU.reduce_min_float(&(lo_pt.d[2]), pt.d[2])
        GPU.reduce_max_float(&(hi_pt.d[0]), pt.d[0])
        GPU.reduce_max_float(&(hi_pt.d[1]), pt.d[1])
        GPU.reduce_max_float(&(hi_pt.d[2]), pt.d[2])
      end )]
    [ StoreAPI:GPU_Scan( name..'_gencodes', s, tblptr, globptr,
                         RowTyp, row, newlist{ code_tmp, id_tmp,
                                               pts, lo_pt, hi_pt },
      quote
        var pt              = pts[row]
        var dx, dy, dz      = (hi_pt.d[0] - lo_pt.d[0]) * 1.00001f,
                              (hi_pt.d[1] - lo_pt.d[1]) * 1.00001f,
                              (hi_pt.d[2] - lo_pt.d[2]) * 1.00001f
        var fx, fy, fz      = pt.d[0] - lo_pt.d[0],
                              pt.d[1] - lo_pt.d[1],
                              pt.d[2] - lo_pt.d[2]
        var x, y, z         = [uint32](fx / dx * 1024.0f),
                              [uint32](fy / dy * 1024.0f),
                              [uint32](fz / dz * 1024.0f)
        -- 98 7654 3210 ->    98 .... .... .... .... 7654 3210
        --              ->    98 .... .... 7654 .... .... 3210
        --              ->    98 .... 76.. ..54 .... 32.. ..10
        --              ->  9..8 ..7. .6.. 5..4 ..3. .2.. 1..0
        x = (x or (x << 16)) and 0x30000FF
        x = (x or (x << 8))  and 0x300F00F
        x = (x or (x << 4))  and 0x30C30C3
        x = (x or (x << 2))  and 0x9249249

        y = (y or (y << 16)) and 0x30000FF
        y = (y or (y << 8))  and 0x300F00F
        y = (y or (y << 4))  and 0x30C30C3
        y = (y or (y << 2))  and 0x9249249

        z = (z or (z << 16)) and 0x30000FF
        z = (z or (z << 8))  and 0x300F00F
        z = (z or (z << 4))  and 0x30C30C3
        z = (z or (z << 2))  and 0x9249249

        var code            = x or (y << 1) or (z << 2)
        code_tmp[row]          = code
        id_tmp[row]         = row
        --GPU.printf("(%6d) code %8x\n", row, code)
      end )]

    -- Kernel(s) 2 : sort the data by Morton code
    GPU.sort32( SIZE, 30, code_tmp, codes, id_tmp, this.cpu_data.ids )
    --this:gpu_debug_print()
    --do
    --  var cpucodes  = [&uint32](C.malloc(SIZE*sizeof(uint32)))
    --  GPU.memcpy_from_gpu(cpucodes, codes, SIZE*sizeof(uint32))
    --  C.printf("CODES\n")
    --  for k=0,SIZE do
    --    C.printf("%6d) %08x\n", k, cpucodes[k])
    --  end
    --  C.free(cpucodes)
    --end

    -- Kernel(s) 3 : compute the BVH-tree nodes
    -- Some code details adapted from Tero Karras
    escape
      assert(key_t == uint32, 'expected u32 key')
      local terra determineRange( codes : &uint32, N : key_t, node_i : key_t )
        -- special case
        if node_i == 0 then return [key_t](0), [key_t](N-1) end
        -- so, assume node_i > 0 and node_i < N-1

        var code          = codes[node_i]
        var prv_code      = codes[node_i-1]
        var nxt_code      = codes[node_i+1]
        -- length of prefix in common
        var nxt_prefix    = GPU.clz_b32(code ^ nxt_code)
        var prv_prefix    = GPU.clz_b32(code ^ prv_code)
        var diff_prefix   = [int32](nxt_prefix) - [int32](prv_prefix)
        var sign : int32  = terralib.select( diff_prefix > 0, 1,
                              terralib.select( diff_prefix < 0, -1, 0 ))
        -- the longer prefix is the side we want to extend the range
        -- into, because those codes are more in common
        -- If this is the previous side, then sign is -1
        -- If this is the next side, then sign is 1

        if sign ~= 0 then
          -- find limit on range
          var lim         = 2
          -- we want to find all codes that have strictly GREATER than
          -- min_pre (prv_prefix if we are extending positively) prefixes
          -- in common with the code at this position
          var min_pre     = prv_prefix
          -- since we are extending positively, we want to step
          -- strictly less than the end of the array
          var max_lim     = N - node_i
          if sign < 0 then
            -- in the negative case, we use the other prefix as minimum
            -- and we want to avoid stepping into negative values
            min_pre       = nxt_prefix
            max_lim       = node_i + 1
          end
          -- here we keep increasing the limit of our search until we
          -- blow out or until we have a prefix that (being less or equal)
          -- (to the lower bound prefix length) is out of the range
          while lim < max_lim do
            var pre       = GPU.clz_b32(code ^ codes[ node_i + sign*lim ])
            if pre <= min_pre then break end
            lim           = lim * 2
          end
          -- clamp limit value
          if lim > max_lim then lim = max_lim end
          -- because we are clamping the limit to the maximum
          -- (which lies outside the index-able range)
          -- and because the other condition is that the limit indexes
          -- an out-of-range value; we are guaranteed that the bound
          -- we want must be a step strictly less than this 'lim'

          -- binary search step for precise end of range
          --var old_lim     = lim
          var step        = 0
          repeat
            -- note this cycles through sequences like...
            --    8, 4, 2, 1
            --    9, 5, 3, 2, 1
            --    11, 6, 3, 2, 1
            lim           = (lim+1) >> 1
            -- and therefore may try to step out of range

            -- candidate update to step
            var maybe     = step + lim
            if maybe < max_lim then
              var maybe_pre = GPU.clz_b32(code ^ codes[node_i + sign*maybe])
              -- if the proposal remains in the prefix code set
              if maybe_pre > min_pre then
                step = maybe
              end
            end
          until lim <= 1
          --GPU.printf("    %6d -(%2d)- lim %6d step %6d \n",
          --           node_i, sign, old_lim, step)
          if sign < 0 then
            return node_i + sign*step, node_i
          else
            return node_i, node_i + sign*step
          end

        else -- the next and previous codes are the same as this code...
          -- In this edge-case we have to find the range of equal
          -- codes and then proceed to split it until we figure out
          -- which split lies right next to this node_i

          -- search out
          var lo_lim      = 2
          while lo_lim <= node_i and codes[node_i-lo_lim] == code do
            lo_lim        = lo_lim * 2
          end
          if lo_lim > node_i then lo_lim = node_i+1 end
          var hi_lim      = 2
          while hi_lim < N-node_i and codes[node_i+hi_lim] == code do
            hi_lim        = hi_lim * 2
          end
          if hi_lim > N-node_i then hi_lim = N-node_i end

          -- and retract back to the precise limits of this range of
          -- equal nodes by binary search
          var lo, hi      = node_i, node_i
          repeat
            lo_lim        = (lo_lim + 1) >> 1
            if lo >= lo_lim and codes[lo-lo_lim] == code then
              lo = lo-lo_lim
            end
          until lo_lim <= 1
          repeat
            hi_lim        = (hi_lim + 1) >> 1
            if hi+hi_lim < N and codes[hi+hi_lim] == code then
              hi = hi + hi_lim
            end
          until hi_lim <= 1

          -- ok, we have the range bounds now
          -- we can do a read-free bisection to finish with the split...
          while hi-lo > 1 do
            -- use method from findSplit below...
            var split     = (lo + hi) >> 1
            if node_i <= split then
              hi          = split
              if node_i == hi then return lo, hi end
            else
              lo          = split+1
              if node_i == lo then return lo, hi end
            end
          end
          return N,N -- error case; should never reach
        end
      end
      local terra findSplit( codes : &uint32, lo : key_t, hi : key_t )
        var lo_code       = codes[lo]
        var hi_pre        = GPU.clz_b32(lo_code ^ codes[hi])
        -- handle edge-case of range with all equal codes
        if hi_pre == 32 then return (lo + hi) >> 1 end

        -- binary search for the split point
        var split         = lo
        var step          = hi - lo
        repeat
          -- note this cycles through sequences like...
          --    8, 4, 2, 1
          --    9, 5, 3, 2, 1
          --    11, 6, 3, 2, 1
          step            = (step+1) >> 1
          -- and therefore may try to step out of range

          var probe       = split + step
          if probe < hi then
            var probe_pre = GPU.clz_b32( lo_code ^ codes[probe] )
            -- if the probe is more similar to lo_code than hi_code, accept
            if probe_pre > hi_pre then
              split       = probe
            end
          end
        until step <= 1

        return split
      end
      -- use the two above functions...
      emit(StoreAPI:GPU_Scan( name..'_gennodes', s, tblptr, globptr,
                              RowTyp, row, newlist{ gpu_data, codes },
      quote
        var N               = gpu_data.n_geom
        -- the internal node to populate at this level/loop-iter
        var node_i          = row
        -- there are only N-1 internal tree nodes.
        if row == N-1 then return end

        -- compute range at this node
        var lo_i, hi_i      = determineRange( codes, N, node_i )

        -- find the split point; sub-ranges will be
        --    [lo_i,split] [split+1,hi_i]
        var split           = findSplit( codes, lo_i, hi_i )
        --GPU.printf("RANGE %6d) [%6d,%6d] @ %6d\n", row, lo_i, hi_i, split)
        
        -- assign node data...
        var left_leaf       = terralib.select(split == lo_i,   N, 0)
        var right_leaf      = terralib.select(split+1 == hi_i, N, 0)
        var left            = split   + left_leaf
        var right           = split+1 + right_leaf
        gpu_data.nodes[node_i].left   = left
        gpu_data.nodes[node_i].right  = right
        --gpu_data.parents[left]        = node_i
        --gpu_data.parents[right]       = node_i
        --if node_i == 0 then gpu_data.parents[0] = 0 end
      end))
    end -- end of escape

    -- Cleanup temporaries from above
    gfree(codes)
    gfree(code_tmp)
    gfree(id_tmp)
    gfree(pts)
    gfree(lo_pt)
    gfree(hi_pt)

    -- Kernel(s) 4 : refresh the tree volumes
    this:gpu_fit_volumes( s )

    -- Set status stuff
    self.gpu_rebuild    = false
    self.gpu_refit      = false
    self.n_updates      = 0
  end

  terra BVH:gpu_update( s : StorePtr )
    --C.printf('** in GPU update: rebuild/refit (%d/%d) n_updates: %d\n',
    --         self.gpu_rebuild, self.gpu_refit, self.n_updates)
    self:bvh_meta_to_cpu()
    if self.gpu_rebuild then
      self:gpu_build(s)
    elseif self.gpu_refit then
      if self.cpu_data.n_geom > 0 then
        if self.n_updates < 7 then
          --self:update_helper(s, ROOT)
          --self.n_updates  = self.n_updates + 1
          self:gpu_build(s)
        else
          self:gpu_build(s)
        end
      end
      self.gpu_refit  = false
    end
  end

  CACHE.GPU_FUNCTIONS_BUILT = true
end

function BVH_BVH_Traversal:_INTERNAL_PreJoinUpdate(
  L_API, R_API, name, storeptr, idxptr0, idxptr1, gpu_tblptr, gpu_globptr
)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  local on_gpu              = not not gpu_tblptr
  if on_gpu then self:_INTERNAL_Construct_GPU_Functions(L_API, R_API) end

  local IS_SAME_IDX         = (self._left == self._right)
  if IS_SAME_IDX then
    if on_gpu then
      return quote
        [idxptr0]:gpu_update( [storeptr] )
      end
    else
      return quote
        [idxptr0]:update( [storeptr] )
      end
    end
  else
    if on_gpu then
      if self._gpu_scan == 'left' then
        return quote
          [idxptr1]:gpu_update( [storeptr] )
        end
      else assert(self._gpu_scan == 'right')
        return quote
          [idxptr0]:gpu_update( [storeptr] )
        end
      end
    else
      return quote
        [idxptr0]:update( [storeptr] )
        [idxptr1]:update( [storeptr] )
      end
    end
  end
end

function BVH_BVH_Traversal:_INTERNAL_Split_LoopGen(L_API, R_API, for_gpu)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  if for_gpu then self:_INTERNAL_Construct_GPU_Functions(L_API, R_API) end
  local CACHE               = self:_INTERNAL_get_CACHE(L_API)
  if for_gpu then
    return CACHE.BVH_BVH_GPU_loopgen
  else
    return CACHE.BVH_BVH_CPU_loopgen
  end
end

function BVH_BVH_Traversal:_INTERNAL_LoopGen(StoreAPI, for_gpu)
  return self:_INTERNAL_Split_LoopGen(StoreAPI, StoreAPI, for_gpu)
end

function BVH_BVH_Traversal:_INTERNAL_Construct_Functions(L_API, R_API)
  local CACHE               = self:_INTERNAL_get_CACHE(L_API)
  if CACHE.TRAV_FUNCTIONS_BUILT then return end

  self._left:_INTERNAL_Construct_Functions(L_API)
  self._right:_INTERNAL_Construct_Functions(R_API)

  local BVH_L               = self._left:_INTERNAL_get_CACHE(L_API).BVH
  local BVH_R               = self._right:_INTERNAL_get_CACHE(R_API).BVH
  local key_t_L             = T.row(self._left._table):terratype()
  local key_t_R             = T.row(self._right._table):terratype()
  local vol_t_L             = self._left._volume:terratype()
  local vol_t_R             = self._right._volume:terratype()

  local struct WorkPacket {
    i_L       : key_t_L
    i_R       : key_t_R
  }

  local IS_SAME_IDX         = (self._left == self._right)

  local isctfn              = L_API:GetCPUFunction( self._vol_isct )

  local function BVH_BVH_CPU_loopgen( storeptr, is_self_join,
                                      idxptr0, idxptr1,
                                      row0sym, row1sym, args, bodycode )
    assert(terralib.issymbol(storeptr), 'INTERNAL: expect symbol')
    local StorePtrType  = storeptr.type

    local terra bvh_loop( s : StorePtrType, bvh_L : &BVH_L, bvh_R : &BVH_R,
                          [args] )
      -- exit early if either operand table is empty
      if bvh_L.geom_ids:size() == 0 or bvh_R.geom_ids:size() == 0 then
        return
      end

      var i_L : key_t_L     = self._left._ROOT
      var i_R : key_t_R     = self._right._ROOT
      var work_stack : stack(WorkPacket)
      work_stack:init()

      var node_L  = bvh_L.bvh_nodes(i_L)
      var node_R  = bvh_R.bvh_nodes(i_R)
      var vol_L   = node_L.vol
      var vol_R   = node_R.vol

      var flip_dir  = true
      var do_pop    = false

      repeat
        if do_pop and not work_stack:is_empty() then
          var packet  = work_stack:pop()
          i_L         = packet.i_L
          i_R         = packet.i_R
          node_L      = bvh_L.bvh_nodes(i_L)
          node_R      = bvh_R.bvh_nodes(i_R)
          vol_L       = node_L.vol
          vol_R       = node_R.vol
        end
        if isctfn(s, vol_L, vol_R) then
          var leaf_L    = node_L:is_leaf()
          var leaf_R    = node_R:is_leaf()
          if leaf_L and leaf_R then
            -- INNER LOOP
            do_pop = true
            var start_L, stop_L = node_L:get_leaf_contents()
            var start_R, stop_R = node_R:get_leaf_contents()
            for k_L = start_L,stop_L do
              vol_L             = bvh_L.geom_vols(k_L)
              var gid_L         = bvh_L.geom_ids(k_L)
              for k_R = start_R,stop_R do
                vol_R           = bvh_R.geom_vols(k_R)
                var gid_R       = bvh_R.geom_ids(k_R)
                var [row0sym]   = gid_L
                var [row1sym]   = gid_R
                -- Handle Self-Joins
                var check       = true
                escape
                  -- Case: duplicates have been eliminated everywhere
                  --       except at leaves intersecting themselves
                  if IS_SAME_IDX and is_self_join then emit quote
                    if i_L == i_R then
                      check = (row0sym <= row1sym)
                    else
                      if row1sym < row0sym then
                        row0sym, row1sym = row1sym, row0sym
                      end
                    end
                  -- Case: because different indices were used, no
                  --       symmetric cases were eliminated in the middle
                  end elseif not IS_SAME_IDX and is_self_join then emit quote
                    check = (row0sym <= row1sym)
                  end end
                end
                -- Process Pair
                if check and isctfn(s, vol_L, vol_R) then
                  var [storeptr]  = s
                  [bodycode]
                end
              end
            end
          elseif IS_SAME_IDX and i_L == i_R then
            escape if IS_SAME_IDX then emit quote
              do_pop      = false
              var node = node_L
              work_stack:push( WorkPacket{ i_L=node.right, i_R=node.right })
              work_stack:push( WorkPacket{ i_L=node.left,  i_R=node.right })
              i_L         = node.left
              i_R         = i_L
              node_L      = bvh_L.bvh_nodes(i_L)
              node_R      = node_L
              vol_L       = node_L.vol
              vol_R       = vol_L
            end end end
          else
            do_pop      = false
            var go_left : bool
            if      leaf_L then go_left = false
            elseif  leaf_R then go_left = true
            else
              flip_dir  = not flip_dir
              go_left   = flip_dir
            end
            if go_left then
              work_stack:push( WorkPacket{ i_L=node_L.right, i_R=i_R })
              i_L       = node_L.left
              node_L    = bvh_L.bvh_nodes(i_L)
              vol_L     = node_L.vol
            else
              work_stack:push( WorkPacket{ i_L=i_L, i_R=node_R.right })
              i_R       = node_R.left
              node_R    = bvh_R.bvh_nodes(i_R)
              vol_R     = node_R.vol
            end
          end
        else
          do_pop = true
        end
      until work_stack:is_empty()

      work_stack:destroy()
    end

    return quote
      bvh_loop( [storeptr], [idxptr0], [idxptr1], [args] )
    end
  end

  CACHE.BVH_BVH_CPU_loopgen = BVH_BVH_CPU_loopgen
  CACHE.TRAV_FUNCTIONS_BUILT   = true
end

function BVH_BVH_Traversal:_INTERNAL_Construct_GPU_Functions(L_API, R_API)
  local CACHE               = self:_INTERNAL_get_CACHE(L_API)
  if CACHE.GPU_TRAV_FUNCTIONS_BUILT then return end

  self._left:_INTERNAL_Construct_GPU_Functions(L_API)
  self._right:_INTERNAL_Construct_GPU_Functions(R_API)

  local BVH_L               = self._left:_INTERNAL_get_CACHE(L_API).BVH
  local BVH_R               = self._right:_INTERNAL_get_CACHE(R_API).BVH
  local key_t_L             = T.row(self._left._table):terratype()
  local key_t_R             = T.row(self._right._table):terratype()
  local vol_t_L             = self._left._volume:terratype()
  local vol_t_R             = self._right._volume:terratype()

  local NULL_R              = (`[key_t_R](-1))

  assert(self._gpu_scan == 'left', 'TODO support non-left gpu scan')
  --local struct WorkPacket {
  --  i_L       : key_t_L
  --  i_R       : key_t_R
  --}

  --local StorePtr            = &(StoreAPI:StoreTyp())
  --local GTbl                = StoreAPI:GPU_TablesTyp()
  --local GGlob               = StoreAPI:GPU_GlobalsTyp()
  local RowTyp0             = T.row(self._left._table)
  local RowTyp1             = T.row(self._right._table)

  local IS_SAME_IDX         = (self._left == self._right)

  local isctfn              = L_API:GetGPUFunction( self._vol_isct )
  local absfn_L             = L_API:GetGPUFunction( self._left._abstract )

  local function BVH_BVH_GPU_loopgen( name, storeptr, is_self_join,
                                      gpu_tblptr, gpu_globptr,
                                      idxptr0, idxptr1,
                                      row0sym, row1sym, args, bodycode )
    if is_self_join then assert(IS_SAME_IDX) end

    local idx_R   = symbol(&BVH_R, 'idxptr1')
    local t_args  = newlist{ storeptr,
                             idx_R, unpack(args) }
    --print('t_args')
    --print(unpack(t_args))
    --print('end')
    local terra bvh_gpu_loop( [t_args] )
      idxptr1:bvh_meta_to_gpu()
      --C.printf("BVH TRAVERSE\n")
      var gpu_data_1    = idx_R.gpu_data

      -- R-blocking
      var N_0           = [ L_API:Size( storeptr, RowTyp0 ) ]
      var N_1           = [ R_API:Size( storeptr, RowTyp1 ) ]
      var N_Rblocks     = [uint32](16)
      -- cut down the factor when reaching better machine occupancy
      if     N_0 >=  20000 then
        N_Rblocks       = 8
      elseif N_0 >=  40000 then
        N_Rblocks       = 4
      elseif N_0 >=  80000 then
        N_Rblocks       = 2
      elseif N_0 >= 160000 then
        N_Rblocks       = 1
      end
      var R_block : uint32
      -- don't block if there's nothing to block
      if N_1 < N_Rblocks * 2 then
        N_Rblocks       = 1
      end

      --C.printf("START %d\n", N_Rblocks)

      -- first, update the leaf node volumes
      [ L_API:GPU_Scan_Multi( name..'_traverse', storeptr,
                              gpu_tblptr, gpu_globptr,
                              N_Rblocks, R_block,
                              RowTyp0, row0sym,
                              newlist{ N_Rblocks, gpu_data_1 },
        quote
          --var do_print = (row0sym % 10000 == 0)
          --if do_print then GPU.printf("HERE for row %d\n", row0sym) end
          var N_R       = gpu_data_1.n_geom

          -- assemble traversal-constant data and scratchpad
          var stack : key_t_R[32]
          stack[0]      = NULL_R
          var stack_i   = 0
          var node_R    = 0
          --var idx_L     = row0sym
          var vol_L     = absfn_L(gpu_tblptr, gpu_globptr, row0sym)
          var iter      = 0

          -- R-blocking
          var block_size = N_R / N_Rblocks
          var R_lo      = R_block * block_size
          var R_hi      = R_lo + block_size
          -- fix rounding difference in last block
          if R_block == N_Rblocks-1 then R_hi = N_R end
          --GPU.printf("R block/Nblock %d %d lo-hi [%d, %d] %d\n",
          --           R_block, N_Rblocks, R_lo, R_hi,
          --           row0sym)

          repeat
            iter = iter + 1
            -- leaf node
            if node_R >= N_R then
              var i_R       = node_R - N_R
              var vol_R     = gpu_data_1.geom_vols[i_R]
              -- test for intersection
              var is_isct = isctfn(gpu_tblptr, gpu_globptr, vol_L, vol_R)
              if is_isct then
                var [row1sym] = gpu_data_1.ids[i_R]
                escape if is_self_join then emit quote
                  if row0sym <= row1sym then
                    --GPU.printf("emit %4d %4d (%4d - %4d)\n",
                    --           row0sym, row1sym,
                    --           R_block, i_R)
                    [bodycode]
                  end
                end else emit quote
                  [bodycode]
                end end end
              end
              --var isct_char = terralib.select(is_isct, 'x', ' ')
              --if do_print then
              --  GPU.printf("[%4d/%4d] %4d:%4d [%4d.%4d] (%s)\n",
              --             row0sym, iter, stack_i, node_R,
              --             row0sym, gpu_data_1.ids[i_R], isct_char)
              --end
              -- pop the stack
              node_R        = stack[stack_i]
              stack_i       = stack_i - 1
            -- internal node
            else
              var i_R       = node_R
              var vol_R     = gpu_data_1.node_vols[i_R]
              var is_isct   = isctfn(gpu_tblptr, gpu_globptr, vol_L, vol_R)
              --if do_print then
              --  GPU.printf("[%4d/%4d] %4d:%4d < %4d %4d >\n",
              --             row0sym, iter, stack_i, i_R,
              --             gpu_data_1.nodes[i_R].left,
              --             gpu_data_1.nodes[i_R].right)
              --end
              if is_isct then
                var node    = gpu_data_1.nodes[i_R]
                var do_L    = R_lo <= node.left
                var do_R    = node.right < R_hi
                if node.left >= N_R then
                  var i = node.left - N_R
                  do_L  = R_lo <= i and i < R_hi
                end
                if node.right >= N_R then
                  var i = node.right - N_R
                  do_R  = R_lo <= i and i < R_hi
                end
                -- postpone right traversal if traversing both
                if do_L and do_R then
                  stack_i   = stack_i + 1
                  stack[stack_i] = node.right
                end
                if do_L then
                  node_R    = node.left
                elseif do_R then
                  node_R    = node.right
                end

                -- var node    = gpu_data_1.nodes[i_R]
                -- -- push the right child and immediately traverse the left
                -- stack_i     = stack_i + 1
                -- stack[stack_i] = node.right
                -- node_R      = node.left
              else
                -- pop the stack
                node_R      = stack[stack_i]
                stack_i     = stack_i - 1
              end
            end
          until node_R == NULL_R
        end) ]
      --C.printf("post-loop!\n    TRAVERSE\n")
    end

    return quote
      bvh_gpu_loop( [storeptr],
                    [idxptr1], [args] )
    end
  end

  CACHE.BVH_BVH_GPU_loopgen = BVH_BVH_GPU_loopgen
  CACHE.GPU_TRAV_FUNCTIONS_BUILT = true
end


-------------------------------------------------------------------------------
--[[                                Exports                                ]]--
-------------------------------------------------------------------------------

Exports.is_bvh_index          = is_bvh_index
Exports.is_bvh_bvh_traversal  = is_bvh_bvh_traversal

Exports.bvh_index             = new_bvh_index
Exports.bvh_bvh_traversal     = new_bvh_bvh_traversal

