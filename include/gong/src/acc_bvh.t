
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

local Schemata      = require 'gong.src.schemata'
local is_table      = Schemata.is_table

local Functions     = require 'gong.src.functions'

local AccStructs    = require 'gong.src.acc_structs'

local newlist       = terralib.newlist

-------------------------------------------------------------------------------
--[[                           Helper Functions                            ]]--
-------------------------------------------------------------------------------

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
    _cpu_enabled    = true,
    _gpu_enabled    = false,
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


function BVH_Index:_INTERNAL_StructLayout()
  if self._CACHE.BVH then return self._CACHE.BVH end

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
  terra BVH:init()
    self.bvh_nodes:init()
    self.geom_ids:init()
    self.geom_vols:init()
    self.n_updates  = 0
    self.rebuild    = true
    self.refit      = true
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

function BVH_Index:_INTERNAL_do_invalidate(idxptr, size_invalid,data_invalid)
  return quote
    idxptr.rebuild  = idxptr.rebuild  or size_invalid
    idxptr.refit    = idxptr.refit    or data_invalid
    --idxptr.rebuild  = true -- hack for now...
  end
end

function BVH_Index:_INTERNAL_PreJoinUpdate( StoreAPI,
                                            name, storeptr, idxptr,
                                            gpu_tblptr, gpu_globptr )
  self:_INTERNAL_Construct_Functions(StoreAPI)

  assert(gpu_tblptr == nil, 'INTERNAL: expect BVH_Index on CPU only')

  return quote [idxptr]:update( [storeptr] ) end
end

function BVH_Index:_INTERNAL_Construct_Functions(StoreAPI)
  if self._CACHE.FUNCTIONS_BUILT then return end

  local ROOT          = self._ROOT
  local RowTyp        = T.row(self._table)
  local key_t         = T.row(self._table):terratype()
  local vol_t         = self._volume:terratype()

  local BVH                 = self._CACHE.BVH
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
    var this    = self
    var k       : key_t
    var SIZE    = [ StoreAPI:Size( s, RowTyp ) ]

    -- extract the bounding volumes to begin with
    this.geom_ids:resize(SIZE)
    this.geom_vols:resize(SIZE)
    this.bvh_nodes:resize(0)
    [ StoreAPI:Scan( s, RowTyp, k, quote
        this.geom_ids(k)    = k
        this.geom_vols(k)   = absfn(s,k)
    end ) ]

    -- recursively build the tree
    if SIZE > 0 then
      assert(ROOT == self:tree_build( s, 0, SIZE, 2 ), 'INTERNAL: bad root')
    end

    self.rebuild    = false
    self.refit      = false
    self.n_updates  = 0
  end

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
        if self.n_updates <= 7 then
          self:update_helper(storeptr, ROOT)
          self.n_updates  = self.n_updates + 1
        else
          self:build(storeptr)
        end
      end
      self.refit  = false
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

  local IS_SAME_IDX         = (self._left == self._right)
  local IS_SAME_TBL         = (self._left._table == self._right._table)

  local isctfn              = StoreAPI:GetCPUFunction( self._vol_isct )

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
                  if IS_SAME_IDX and IS_SAME_TBL then emit quote
                    if i_L == i_R then
                      check = (row0sym <= row1sym)
                    else
                      if row1sym < row0sym then
                        row0sym, row1sym = row1sym, row0sym
                      end
                    end
                  -- Case: because different indices were used, no
                  --       symmetric cases were eliminated in the middle
                  end elseif not IS_SAME_IDX and IS_SAME_TBL then emit quote
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
      bvh_loop( [storeptr], [idxptr0], [idxptr1] )
    end
  end

  self._CACHE.BVH_BVH_loopgen = BVH_BVH_loopgen
  self._CACHE.FUNCTIONS_BUILT   = true
end



-------------------------------------------------------------------------------
--[[                                Exports                                ]]--
-------------------------------------------------------------------------------

Exports.is_bvh_index          = is_bvh_index
Exports.is_bvh_bvh_traversal  = is_bvh_bvh_traversal

Exports.bvh_index             = new_bvh_index
Exports.bvh_bvh_traversal     = new_bvh_bvh_traversal

