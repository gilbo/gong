--import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.datastore"] = Exports

-------------------------------------------------------------------------------

local T             = require 'gong.src.types'
local Util          = require 'gong.src.util'
local is_type       = T.is_type
--local SrcInfo       = Util.SrcInfo
--local NewSymbol     = Util.NewSymbol
--local is_symbol     = Util.is_symbol
local is_id_str     = Util.is_id_str
--local is_int        = Util.is_int
local INTERNAL_ERR  = Util.INTERNAL_ERR

local Schemata      = require 'gong.src.schemata'
local is_table      = Schemata.is_table
local is_field      = Schemata.is_field
local is_index      = Schemata.is_index

local AccStructs    = require 'gong.src.acc_structs'

local CodeGen       = require 'gong.src.codegen'

local Functions     = require 'gong.src.functions'

local Profiling     = require 'gong.src.profiling'
local NewProfiler   = Profiling.NewProfiler

-----------------------------------------

local C             = require 'gong.src.c'
local assert        = C.assert

local StdContainers = require 'gong.src.stdcontainers'
local vector        = StdContainers.vector

local PARAMETER     = (require 'gong.src.params').get_param
local verbosity     = require('gong.src.verbosity').get_verbosity()

local INDEX_REBUILD_FRACTION = PARAMETER('INDEX_REBUILD_FRACTION')

local GPU_ENABLED   = PARAMETER('GPU_ENABLED')
local GPU_DataStore = require 'gong.src.gpu_datastore'

local newlist       = terralib.newlist

-------------------------------------------------------------------------------

--[[

  Sketch of Current Storage Policy.

    This sketch is provided because tracing through a bunch of code
    that metaprograms data-structure layout from the schematic description
    is a brain-melting activity.  Really, we just need a clear picture
    of what we're trying to do, and then to do it.
  
  Here is a grammatical summary of the abstract data model
    <Schema>  ::= (<Table> `;`)* (<Global> *)
    <Table>   ::= <name> `{` <Field>* `}`
    <Field>   ::= <name> `:` <Type>
    <Index>   ::= <name> `:` <Field>+ `->` <Table>
    <Global>  ::= <name> `:` <Type>
    <Type>    ::= <primitive>
                | `row(` <Table> `)`
                | `tensor(` <Type> `,` <num>+ `)`
                | `record{` (<name> `:` <Type>)* `}`
  
  We're going to translate these down into C-data structures according to
  the following intuition:
    Schema      |-->    CStore
    Table       |-->    CTable
    Field       |-->    Column*  (we use a naive layout right now)
    Index       |-->    CIndex
    Global      |-->    CGlobal

  Specifically, a CStore, and CTable look like the following

    CStore {
      <table_name_0>  : CTable_0
      <table_name_1>  : CTable_1
      ...
      <global_name_0> : <type_0>
      <global_name_1> : <type_1>
      ...
    }
    
    CTable_i {
      alloc_size      : Size_Type_i     -- allocated space
      size            : Size_Type_i     -- used space
      <col_name_0>    : &<type_0>
      <col_name_1>    : &<type_1>
      ...
    }
    If this table is setup for individual row deletion too, then...
    CTable_i {
      n_alloc         : Size_Type_i
      n_row           : Size_Type_i
      free_list       : Key_Type_i
      is_compact      : bool
      (columns as before; one of these is used to hide the free-list)
      is_live_column  : &bool
    }

    CIndex = GeneralMergeIndex {
      keys            : &vector({key1, dst_key})
    }

  This is insufficiently sophisticated for some purposes, but will work
  fine for a first-pass implementation.

  These structures must support certain internal operations in addition to
  specifying a data layout.

      Scan( Table, row, body )
      Read( Table, row, path )
     Write( Table, row, path, rval )
    Reduce( Table, op, row, path, rval )
    Insert( Table, vals )
     Merge( Table, row, k0, k1, body, else_vals ) -- requires deletions

  We Additionally want to support external API operations
    
    Key<Table_i> = blah

    NewStore() : CStore
    DestroyStore( CStore )
    GetError( CStore )
    
    -- BulkLoad
    BeginLoad_<Table_i>( CStore, SizeT_i )
      LoadRow_<Table_i>( CStore, FieldT_0, FieldT_1, ... )
    EndLoad_<Table_i>( CStore )

    -- Point Access
    Read_<Table_i>_<Field_j>( CStore, Key ) : FieldT
    Write_<Table_i>_<Field_j>( CStore, Key, FieldT )
    -- ReadWrite Lock
    ReadWriteLock_<Table_i>_<Field_j>( CStore ) : &FieldT
    ReadWriteUnlock_<Table_i>_<Field_j>( CStore )
    -- Read Lock
    ReadLock_<Table_i>_<Field_j>( CStore ) : &FieldT
    ReadUnlock_<Table_i>_<Field_j>( CStore )
    
--]]

-------------------------------------------------------------------------------
--[[                               Preamble                                ]]--
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

local realloc_ptr = macro(function(ptr, typ, N)
  typ = assert(typ:astype())
  return quote [ptr] = [&typ](C.realloc( [ptr], sizeof(typ) * [N] )) end
end)

local alloc_ptr = macro(function(typ,N)
  typ = assert(typ:astype())
  return `[&typ](C.malloc( sizeof(typ) * [N] ))
end)

local WRAPPER_ROOT  = {}
local ERR_BUF_LEN   = 2048


-------------------------------------------------------------------------------
--[[                           GeneralMergeIndex                           ]]--
-------------------------------------------------------------------------------

local function PairHelper(K1,KDST)
  local struct Pair {
    k1    : K1
    dst   : KDST
  }

  Pair.metamethods.__lt = macro(function(a,b)
    if a:gettype() == Pair then   a = `a.k1
    else assert(a:gettype() == K1, 'INTERNAL: expect key type') end
    if b:gettype() == Pair then   b = `b.k1
    else assert(b:gettype() == K1, 'INTERNAL: expect key type') end
    return `(a < b)
  end)
  Pair.metamethods.__gt = macro(function(a,b)
    if a:gettype() == Pair then   a = `a.k1
    else assert(a:gettype() == K1, 'INTERNAL: expect key type') end
    if b:gettype() == Pair then   b = `b.k1
    else assert(b:gettype() == K1, 'INTERNAL: expect key type') end
    return `(a > b)
  end)
  Pair.metamethods.__le = macro(function(a,b)
    return `not (a > b)
  end)
  Pair.metamethods.__ge = macro(function(a,b)
    return `not (a < b)
  end)
  Pair.metamethods.__eq = macro(function(a,b)
    if a:gettype() == Pair then   a = `a.k1
    else assert(a:gettype() == K1, 'INTERNAL: expect key type') end
    if b:gettype() == Pair then   b = `b.k1
    else assert(b:gettype() == K1, 'INTERNAL: expect key type') end
    return `(a == b)
  end)
  Pair.metamethods.__ne = macro(function(a,b)
    return `not (a == b)
  end)

  return Pair
end
PairHelper = Util.memoize(PairHelper)

local function GeneralMergeIndex(Index, IndexName)
  local DST             = Index:dst()
  assert(#Index:srcs() == 2, "INTERNAL: expect two keys for index")
  local F0, F1          = unpack(Index:srcs())
  local T0              = F0:type():table()

  assert(not T0:_INTERNAL_HasMergeIndex(),
         "INTERNAL: expect index-source tables to not be merged into")
  assert(not F1:type():table():_INTERNAL_HasMergeIndex(),
         "INTERNAL: expect index-source tables to not be merged into")
  assert(DST:_INTERNAL_HasMergeIndex(),
         "INTERNAL: expect index-destination table to be merged into")

  local KDST            = DST:_INTERNAL_terra_key_type()
  local K0, K1          = F0:type():terratype(), F1:type():terratype()
  local S0, S1          = K0, K1 -- size types

  local Pair            = PairHelper(K1,KDST)

  local CIdx            = terralib.types.newstruct(IndexName)
  CIdx.entries:insertall {
    -- implicit size and alloced
    { '_keys', &(vector(Pair)) }
  }

  local function InstallMethods(W)
    -- pack into the wrapper...
    W._c_cache[T0].parallel_indices:insert(Index)
    W._c_cache[DST].dst_indices:insert(Index)

    local vec_Pair      = vector(Pair)

    local CStore        = W._c_cache[WRAPPER_ROOT].ctype
    local T0name        = W._c_cache[T0].name
    local Dname         = W._c_cache[DST].name
    local F0name        = W._c_cache[F0].name
    local F1name        = W._c_cache[F1].name

    terra CIdx:init( store : &CStore )
      assert(store.[T0name]._size == 0)
      var nalloc        = store.[T0name]._alloc_size
      self._keys        = alloc_ptr( vec_Pair, nalloc )
    end
    terra CIdx:destroy( store : &CStore )
      var size          = store.[T0name]._size
      for k=0,size do   self._keys[k]:destroy()   end
      C.free(self._keys)
      self._keys = nil
    end
    terra CIdx:resize( store    : &CStore,
                       newsize  : S0,   oldsize   : S0,
                       newalloc : S0,   oldalloc  : S0 )
      if newalloc ~= oldalloc then
        realloc_ptr(self._keys, vec_Pair, newalloc)
      end
      if newsize < oldsize then
        for k=newsize,oldsize do self._keys[k]:destroy() end
      elseif newsize > oldsize then
        for k=oldsize,newsize do self._keys[k]:init() end
      end
    end
    terra CIdx:clear( store : &CStore )
      var size          = store.[T0name]._size
      for k=0,size do   self._keys[k]:resize(0)   end
    end

    -- binary search
    local terra find( self : &CIdx, k0 : K0, k1 : K1 )
      var v       = (self._keys + k0)
      var N       = v:size()
      var lo,hi   = int32(0),int32(N)-1
      --C.printf('find %d %d ; %d\n', k0, k1, N)
      while lo <= hi do
        var mid   = lo + (hi-lo)/2
        var mval  = v(mid)
        if mval < k1 then
          lo      = mid+1
        elseif mval > k1 then
          hi      = mid-1
        else -- mid == k1
          return true, v, mid
        end
      end
      assert(lo == hi + 1,
        'INTERNAL: sanity 1 for index find at (%d,%d) lo,hi=%d,%d',
        k0,k1, lo,hi)
      assert(lo >= 0,
        'INTERNAL: sanity 2 for index find at (%d,%d) lo,hi=%d,%d',
        k0,k1, lo,hi)
      return false, v, lo
    end

    terra CIdx:debug_print( store : &CStore )
      var size          = store.[T0name]._size
      for k=0,size do
        C.printf("  %5d:", k)
        var V = self._keys[k]
        for i=0,V:size() do C.printf(" %d(%d)", V(i).k1, V(i).dst) end
        C.printf("\n")
      end
    end
    terra CIdx:check_valid( store : &CStore )
      C.printf("* + * + * Do Validity Check\n")
      var T0size        = store.[T0name]._size
      var T1size        = store.[ W._c_cache[F1:type():table()].name ]._size
      for k0=0,T0size do
        var V   = self._keys[k0]
        for i=0,V:size() do
          var k1  = V(i).k1
          var dst = V(i).dst
          assert(k1 < T1size,
            'INTERNAL: bad key-1 value in index; (%d,%d)=%d', k0, k1, dst)
          assert( [ W:_INTERNAL_is_live_read(store, T.row(DST), dst) ],
            'INTERNAL: dst value is a dead row; (%d,%d)=%d', k0, k1, dst)
          var d0  = (store.[Dname].[F0name])[dst]
          var d1  = (store.[Dname].[F1name])[dst]
          assert(d0 == k0 and d1 == k1,
          'INTERNAL: index key values mismatch at dst; (%d,%d)=%d -> (%d,%d)',
            k0, k1, dst, d0, d1)
        end
      end
    end

    terra CIdx:insert( k0 : K0, k1 : K1, row : KDST )
      var success, v, i = find(self, k0, k1)
      assert(not success, "INTERNAL: expect insert-entry to be missing")
      var N             = v:size()
      v:resize(N+1)
      -- shift contents right and then enter data
      var tmp = v(i)
      for k=i+1,N+1 do v(k), tmp = tmp, v(k) end
      v(i).k1           = k1
      v(i).dst          = row
    end
    terra CIdx:remove( k0 : K0, k1 : K1 )
      var success, v, i = find(self, k0, k1)
      assert(success, "INTERNAL: expect remove-entry to be present")
      var N             = v:size()
      -- shift contents left
      for k=i,N-1 do v(k) = v(k+1) end
      v:resize(N-1)
    end
    terra CIdx:lookup( k0 : K0, k1 : K1 )
      var success, v, i = find(self, k0, k1)
      if success then
        return true, &(v(i).dst)
      else
        var N             = v:size()
        v:resize(N+1)
        -- shift contents right and then enter data
        var tmp = v(i)
        for k=i+1,N+1 do v(k), tmp = tmp, v(k) end
        v(i).k1           = k1
        return false, &(v(i).dst)
      end
    end

    function CIdx.methods.ScanSorted(self, store, k0, k1, dst, body)
      return quote
        var SIZE          = store.[T0name]._size
        var [dst]
        for [k0] = 0, SIZE do
          var v           = self._keys[k0]
          for i=0,v:size() do
            var [k1]      = v(i).k1
            var [dst]     = &(v(i).dst)
            [body]
          end
        end
      end
    end

    local row = symbol(KDST, 'row')
    terra CIdx:rebuild( store : &CStore )
      var this = self
      this:clear(store)
      [ W:Scan( store, T.row(DST), row, quote
          var k0          = (store.[Dname].[F0name])[row]
          var k1          = (store.[Dname].[F1name])[row]
          this:insert(k0, k1, row)
        end) ]
    end

    CIdx.methods.InstallMethods = nil -- erase
  end
  CIdx.methods.InstallMethods = InstallMethods

  return CIdx
end
--GeneralMergeIndex = Util.memoize(GeneralMergeIndex)

-------------------------------------------------------------------------------
--[[                           Wrapper Structure                           ]]--
-------------------------------------------------------------------------------

local Wrapper       = {}
Wrapper.__index     = Wrapper

local function GenerateWrapperStructs(prefix, W)
  -- generate structures
  local CStore              = terralib.types.newstruct('InternalStore')
  -- needlessly defensive padding
  CStore.entries:insert{ '__padding__', uint64 }
  for iTable,Table in ipairs(W._tables) do
    local TableName         = Table:name()..'_'..(iTable-1)
    local CTable            = terralib.types.newstruct(TableName)
    local KeyT              = Table:_INTERNAL_terra_key_type()
    local SizeT             = Table:_INTERNAL_terra_size_type()

    if Table._is_live then
      -- free-list row-management
      CTable.entries:insertall {
        { '_n_alloc',     SizeT },
        { '_n_rows',      SizeT },
        { '_free_list',   KeyT },
        { '_is_compact',  bool },
        { '_is_sorted',   bool },
      }
      assert(#Table:primary_key() == 2, 'expect primary keys')
      local k0type          = Table:primary_key()[1]:type():terratype()
      assert(k0type == KeyT, 'INTERNAL: expected consistent key types')
    else
      -- basic dynamic vector row-management
      CTable.entries:insertall {
        { '_alloc_size',  SizeT },
        { '_size',        SizeT },
      }
    end

    local fields            = Table:_INTERNAL_fields()
    for iField,Field in ipairs(fields) do
      -- blah
      local FieldName       = Field:name()..'_'..(iField-1)
      local FPtr            = &(Field:type():terratype())
      CTable.entries:insert { '_'..FieldName, FPtr }

      W._c_cache[Field]     = { ctype = FPtr,   name = '_'..FieldName }
    end

    CTable:complete()
    CStore.entries:insert { '_'..TableName, CTable }
    W._c_cache[Table]       = { ctype             = CTable,
                                name              = '_'..TableName,
                                parallel_indices  = newlist(),
                                dst_indices       = newlist(),
                                acc_indices       = newlist(), }
    W._type_reverse[CTable] = Table
  end

  -- Add Indices in
  for iIndex,Index in ipairs(W._indices) do
    local IndexName         = Index:name()..'_index'..(iIndex-1)
    local CIndex            = GeneralMergeIndex(Index, IndexName)

    CStore.entries:insert { '_'..IndexName, CIndex }
    W._c_cache[Index]       = { ctype = CIndex, name = '_'..IndexName }
    W._type_reverse[CIndex] = Index
  end

  -- Add Acceleration Indices in
  for iAIndex,AIndex in ipairs(W._acc_indices) do
    assert(AccStructs.is_spatial_index(AIndex),
           'INTERNAL: expected acceleration index')
    local Table             = AIndex:table()
    local AIndexName        = AIndex:name()..'_acc_index'..(iAIndex-1)
    local CAIndex           = AIndex:_INTERNAL_StructLayout(W:GetAccAPI())

    CStore.entries:insert { '_'..AIndexName, CAIndex }
    W._c_cache[AIndex]      = { ctype = CAIndex, name = '_'..AIndexName }
    W._type_reverse[CAIndex]= AIndex

    W._c_cache[Table].acc_indices:insert(AIndex)
  end

  -- Add Globals in
  for iGlobal,Global in ipairs(W._globals) do
    local GlobalName        = Global:name()..'_global'..(iGlobal-1)
    local CGlobal           = Global:type():terratype()

    CStore.entries:insert { '_'..GlobalName, CGlobal }
    W._c_cache[Global]      = { ctype = CGlobal, name = '_'..GlobalName }
    W._type_reverse[CGlobal] = Global
  end

  if W._use_gpu then
    local GW = W._gpu_wrapper
    CStore.entries:insert { '_gpu_data', GW:GPU_Struct() }
  end

  CStore.entries:insertall {
    { '_profiler',      W._profiler:handle() },
    { '_error_msg',     rawstring },
    { '_error_buf',     int8[ERR_BUF_LEN] },
    { '_load_counter',  int64 },
    { '_load_col_counter', int64 },
  }
  CStore:complete()
  W._c_cache[WRAPPER_ROOT]  = { ctype = CStore, name = nil }
  W._type_reverse[CStore]   = WRAPPER_ROOT
end

local function InstallStructMethods(prefix, W)
  local CStore                = W._c_cache[WRAPPER_ROOT].ctype

  for iIndex,Index in ipairs(W._indices) do
    local CIndex            = W._c_cache[Index].ctype
    CIndex.methods.InstallMethods(W)
  end

  -- Per-Table Methods
  for iTable,Table in ipairs(W._tables) do
    local CTable            = W._c_cache[Table].ctype
    local KeyT              = Table:_INTERNAL_terra_key_type()
    local SizeT             = Table:_INTERNAL_terra_size_type()
    local MAXsize           = Table:_INTERNAL_terra_MAX_SIZE()
    local indices           = W._c_cache[Table].parallel_indices

    if Table._is_live then
      CTable.methods.n_rows = macro(function(self) return `self._n_rows end)
      CTable.methods.n_alloc = macro(function(self) return `self._n_alloc end)
      CTable.methods.is_compact = macro(function(self)
                                          return `self._is_compact end)
      CTable.methods.is_sorted = macro(function(self)
                                          return `self._is_sorted end)

      assert(#indices == 0, 'INTERNAL: expect no indices from is_live table')
      local f0, f1          = unpack(Table:primary_key())
      local f0name, f1name  = W._c_cache[f0].name, W._c_cache[f1].name
      local Tf0, Tf1        = f0:type():terratype(), f1:type():terratype()

      local indices         = W._c_cache[Table].dst_indices
      assert(#indices == 1, 'INTERNAL: expect exactly 1 index')
      local Index           = indices[1]
      local CIndex          = W._c_cache[Index].ctype
      local iname           = W._c_cache[Index].name

      local is_live_f = macro(function( store, k )
        return W:_INTERNAL_is_live_read(store, T.row(Table), k)
      end)
      local livename        = W._c_cache[Table._is_live].name
      terra CTable:debug_is_live()
        var this            = self
        var n_alloc         = this._n_alloc
        var is_live         = this.[livename]
        var i = 0
        while i < n_alloc do
          C.printf("%6d: ", i)
          for j=0,50 do
            if i+j < n_alloc then
              if is_live[i+j] then C.printf("1")
                              else C.printf("0") end
            end
          end
          i = i+50
          C.printf('\n')
        end
      end

      terra CTable:alloc_more( store : &CStore, newalloc : SizeT )
        var this              = self
        var oldalloc          = self._n_alloc
        self._n_alloc         = newalloc
        assert(newalloc > oldalloc, 'INTERNAL: no need to alloc_more')
        if self._free_list ~= oldalloc then
          var last_row        = oldalloc-1
          assert(not is_live_f(store,last_row),
                 ['INTERNAL: expect empty free-list OR '..
                  'that the last alloced row is free'])
          assert((this.[f0name])[last_row] == oldalloc,
                 ['INTERNAL: expect last alloced row to be the end of '..
                  'the free list'])
        end

        -- basic reallocation of memory arrays
        escape for iField,Field in ipairs(Table:_INTERNAL_fields()) do
          local fname       = W._c_cache[Field].name
          local ftype       = Field:type():terratype()
          emit quote
            realloc_ptr( this.[fname], ftype, newalloc )
          end
        end end

        -- intialize the hidden free-list, and mark the new rows as dead
        for k=oldalloc,newalloc do
          (this.[f0name])[k] = k+1
          [ W:_INTERNAL_set_dead(store, T.row(Table), k) ]
        end
      end
      terra CTable:clear( store : &CStore )
        var ALLOC             = self._n_alloc
        self._n_rows          = 0

        -- mark all as dead and re-thread free-list
        self._free_list       = 0
        for k=0,ALLOC do
          (self.[f0name])[k] = k+1
          [ W:_INTERNAL_set_dead(store, T.row(Table), k) ]
        end
        -- and now it's de-facto compact again
        self._is_compact      = true
        self._is_sorted       = true
      end
      terra CTable:compact( store : &CStore )
        var N_LIVE            = self._n_rows
        -- strategy: scan the live field until we find the
        --           last live row.  We'll work back from there
        var live_count        = 0
        var live_bound        = 0
        while live_count < N_LIVE do
          var live = is_live_f(store, live_bound)
          if live then live_count = live_count + 1 end
          live_bound = live_bound + 1
        end
        -- short-circuit
        if live_bound - N_LIVE == 0 then
          self._is_compact = true
          return
        end

        var update_idx  = ([double](N_LIVE)/live_bound)
                              >= INDEX_REBUILD_FRACTION

        -- do the row copies we need in order to compact
        var n_to_move         = live_bound - N_LIVE
        var write             = 0
        var read              = live_bound - 1
        while n_to_move > 0 do
          -- start by finding a row to copy and an empty slot to copy it to
          while is_live_f(store, write)    do write = write + 1 end
          while not is_live_f(store, read) do
            -- skipping dead rows at the end counts against
            -- the total number of rows we need to copy
            n_to_move   = n_to_move - 1
            read        = read - 1
          end
          if n_to_move > 0 then
            assert(write < read, 'INTERNAL: Out of Order Copy')
            -- copy row
            escape for iField,Field in ipairs(Table:_INTERNAL_fields()) do
              local fname       = W._c_cache[Field].name
              emit quote
                (self.[fname])[write] = (self.[fname])[read]
              end
            end end
            -- Make sure to set the written row as live
            [ W:_INTERNAL_set_live(store, T.row(Table), write) ]
            -- and repair the index incrementally
            var k0, k1          = self.[f0name][read], self.[f1name][read]
            var found, ptr      = store.[iname]:lookup(k0,k1)
            assert(found, 'INTERNAL: bad index update on compaction')
            @ptr                = write
            -- and adjust all the pointers to continue
            write               = write + 1
            read                = read - 1
            n_to_move           = n_to_move - 1
          end
        end

        -- finally, we need to flush out the dead space
        var ALLOC             = self._n_alloc
        self._free_list       = N_LIVE
        for k=N_LIVE, ALLOC do
          (self.[f0name])[k] = k+1
          [ W:_INTERNAL_set_dead(store, T.row(Table), k) ]
        end
        -- and now the storage is compact again
        self._is_compact      = true
        self._is_sorted       = false
      end
      terra CTable:sort( store : &CStore )
        assert(self:is_compact(), 'INTERNAL: must be compact to sort')
        var N_ALLOC           = self._n_alloc
        -- strategy: create new arrays to dump the re-ordered rows into
        --           and get the new locations by scanning over the index
        var write : KeyT      = 0
        var read : KeyT       = 0
        escape
          local k0, k1        = symbol(Tf0,'k0'), symbol(Tf1,'k1')
          local dst           = symbol(&KeyT)
          -- collect actions to perform for each field
          local finits        = newlist()
          local fcopies       = newlist()
          local fswaps        = newlist()
          for iField,Field in ipairs(Table:_INTERNAL_fields()) do
            -- NOTE: it's very important that we exclude
            -- Table._is_live, because if we re-allocate that field
            -- specifically, then we must make sure to zero out all
            -- the un-used rows, which the generic copy doesn't do
            if Field ~= f0 and Field ~= f1 and Field ~= Table._is_live then
              local fname     = W._c_cache[Field].name
              local ftype     = Field:type():terratype()
              local fsym      = symbol(&ftype, fname..'_newptr')
              finits:insert(quote
                var [fsym]    = alloc_ptr(ftype, N_ALLOC)
              end)
              fcopies:insert(quote
                [fsym][write] = self.[fname][read]
              end)
              fswaps:insert(quote
                C.free(self.[fname])
                self.[fname]  = [fsym]
              end)
            end
          end
          -- do the loop that actually copies...
          emit quote [finits] end
          emit(CIndex.methods.ScanSorted((`store.[iname]), store, k0,k1,dst,
          quote
            read              = @dst
            @dst              = write
            -- note that we can overwrite the existing key arrays
            -- as a special case because we can use the index as a
            -- compressed reference for their values
            self.[f0name][write]  = k0
            self.[f1name][write]  = k1
            [fcopies]
            write             = write + 1
          end))
          emit quote [fswaps] end
        end
        self._is_sorted       = true
      end

    else
      CTable.methods.size   = macro(function(self) return `self._size end)

      terra CTable:resize( store : &CStore, newsize : SizeT )
        var this              = self
        var oldalloc          = self._alloc_size
        var oldsize           = self._size
        self._size            = newsize
        var newalloc          = oldalloc
        if newsize > oldalloc then
          newalloc            = maxf(oldalloc*2, newsize)
          self._alloc_size    = newalloc
          assert(newalloc < MAXsize and newalloc > oldalloc,
                 'exceeded max allowable size on resize()')
          escape for iField,Field in ipairs(Table:_INTERNAL_fields()) do
            local fname       = W._c_cache[Field].name
            local ftype       = Field:type():terratype()
            emit quote
              realloc_ptr( this.[fname], ftype, newalloc )
            end
          end end
        end
        escape for _,Index in ipairs(indices) do
          local iname       = W._c_cache[Index].name
          emit quote store.[iname]:resize(store, newsize,   oldsize,
                                                 newalloc,  oldalloc) end
        end end
      end
    end
  end

  -- Error Handling
  CStore.methods.error        = macro(function(store,errstr,...)
    if not store:gettype():ispointer() then store = `&store end

    errstr = errstr and errstr:asvalue()
    errstr = (type(errstr) == 'string' and errstr) or ""
    local fmtstr = prefix..'Gong Error: '..errstr.."\n"

    local a = newlist{...}
    return quote
      if store._error_msg == nil then
        store._error_msg = &(store._error_buf[0])
        store._error_buf[0] = 0 -- empty string
      end
      var err_used          = C.strlen(store._error_msg)
      var bytes_left        = ERR_BUF_LEN - err_used
      var buf_head          = store._error_msg + err_used
      C.snprintf( buf_head, bytes_left, fmtstr, [a] )
    end
  end)
  CStore.methods.clear_error  = macro(function(store)
    return quote store._error_msg = nil end
  end)
  CStore.methods.get_error    = macro(function(store)
    return `store._error_msg
  end)

  -- Initialization and Destruction
  local MIN_INIT_SIZE = 8
  terra CStore:init()
    var this                = self
    this._profiler:init()
    this._error_msg         = nil
    this._load_counter      = -1
    this._load_col_counter  = -1
    escape for iTable,Table in ipairs(W._tables) do
      local tblname           = W._c_cache[Table].name

      if Table._is_live then
        emit quote
          this.[tblname]._n_alloc     = MIN_INIT_SIZE
          this.[tblname]._n_rows      = 0
          this.[tblname]._free_list   = 0
          this.[tblname]._is_compact  = true -- empty is compact
          this.[tblname]._is_sorted   = true
        end
      else
        emit quote
          this.[tblname]._alloc_size  = MIN_INIT_SIZE
          this.[tblname]._size        = 0
        end
      end

      local fields            = Table:_INTERNAL_fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        local ftype           = Field:type():terratype()
        emit quote
          this.[tblname].[fname] = alloc_ptr(ftype, MIN_INIT_SIZE)
        end
      end

      if Table._is_live then -- init free list
        assert(#Table:primary_key() == 2, 'INTERNAL')
        local f0              = Table:primary_key()[1]
        local f0name          = W._c_cache[f0].name
        emit quote for k=0,MIN_INIT_SIZE do
          (this.[tblname].[f0name])[k] = k+1
        end end
        -- Need to zero out the dead rows...
        emit quote for k=0,MIN_INIT_SIZE do
          [ W:_INTERNAL_set_dead( this, T.row(Table), k ) ]
        end end
      end
    end for iIndex, Index in ipairs(W._indices) do
      local idxname           = W._c_cache[Index].name
      emit quote
        this.[idxname]:init( this )
      end
    end for iAIndex, AIndex in ipairs(W._acc_indices) do
      local aname             = W._c_cache[AIndex].name
      emit quote
        this.[aname]:init()
      end
    end for iGlobal, Global in ipairs(W._globals) do
      local globname          = W._c_cache[Global].name
      local initval           = Global:initval()
      if initval then
        initval               = T.lua_to_terraval(initval, Global:type())
        -- MUST convert to a constant to ensure proper
        -- inlining of the value in the Terra code
        initval               = terralib.constant(Global:type():terratype(),
                                                  initval)
        emit quote
          this.[globname]     = [initval]
        end
      end
    end if W._use_gpu then emit quote
      this._gpu_data:init()
    end end end
  end
  terra CStore:destroy()
    var this        = self
    this._profiler:destroy()
    this._error_msg = nil
    escape for iAIndex, AIndex in ipairs(W._acc_indices) do
      local aname             = W._c_cache[AIndex].name
      emit quote
        this.[aname]:destroy()
      end
    end for iIndex, Index in ipairs(W._indices) do
      local idxname           = W._c_cache[Index].name
      emit quote
        this.[idxname]:destroy( this )
      end
    end for iTable,Table in ipairs(W._tables) do
      local tblname           = W._c_cache[Table].name
      local fields            = Table:_INTERNAL_fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        emit quote
          C.free( this.[tblname].[fname] )
          this.[tblname].[fname] = nil
        end
      end
    end if W._use_gpu then emit quote
      this._gpu_data:destroy()
    end end end
  end
end

local function NewWrapper(args)
  local prefix = assert(args.prefix,'INTERNAL')..'_'

  local W = setmetatable({
    _tables         = assert(args.tables,'INTERNAL'):copy(),
    _indices        = assert(args.indices,'INTERNAL'):copy(),
    _acc_indices    = assert(args.acc_indices,'INTERNAL'):copy(),
    _globals        = assert(args.globals,'INTERNAL'):copy(),
    _use_gpu        = args.use_gpu, -- boolean
    _c_cache        = {},
    _type_reverse   = {},
    _func_cache     = {},
    _profiler       = NewProfiler(),
  }, Wrapper)

  if W._use_gpu then
    W._gpu_wrapper = GPU_DataStore.GenerateSubWrapper(args)
  end
  GenerateWrapperStructs(prefix, W)
  InstallStructMethods(prefix, W)

  if W._use_gpu then
    GPU_DataStore.Install_Extensions(W._gpu_wrapper, W, WRAPPER_ROOT)
  end

  return W
end
Exports.GenerateDataWrapper = NewWrapper


-------------------------------------------------------------------------------
--[[                       Code Generation Interface                       ]]--
-------------------------------------------------------------------------------

local function gpu_name(f_obj)
  local name = f_obj:getname()
  return name..'_GPU'
end

function Wrapper:get_terra_function(f_obj, for_gpu)
  assert(for_gpu ~= nil, 'INTERNAL: expect non-nil gpu flag')
  local name = f_obj:getname()
  if for_gpu then name = gpu_name(f_obj) end

  if not self._func_cache[f_obj] then
    self._func_cache[f_obj] = {}
  end
  local token     = (for_gpu and 'GPU') or 'CPU'

  local traversal = nil
  if f_obj.get_cpu_traversal and not for_gpu then
    traversal     = f_obj:get_cpu_traversal()
  elseif f_obj.get_gpu_traversal and for_gpu then
    traversal     = f_obj:get_gpu_traversal()
  end
  local buffer_effects  = Functions.is_join(f_obj) and
                          f_obj:_INTERNAL_are_cpu_effects_buffered() and
                          not for_gpu
  local buffer_index    = Functions.is_join(f_obj) and
                          f_obj:_INTERNAL_are_cpu_indices_buffered() and
                          not for_gpu
  local verify_index    = Functions.is_join(f_obj) and
                          f_obj:_INTERNAL_verify_index()

  if not self._func_cache[f_obj][token] then
    local tfunc = CodeGen.codegen {
      name              = name,
      ast               = f_obj:_INTERNAL_getast(),
      effects           = f_obj:_INTERNAL_geteffects(),
      traversal         = traversal,
      storewrap         = self,
      for_gpu           = for_gpu,
      buffer_effects    = buffer_effects,
      buffer_index      = buffer_index,
      verify_index      = verify_index,
    }
    self._func_cache[f_obj][token] = tfunc
  end
  return self._func_cache[f_obj][token]
end

-------------------------------------------------------------------------------
--[[        Wrapper Helper Functions; INTERNAL to DataStore module         ]]--
-------------------------------------------------------------------------------

local function unpack_tbl(W, tbltype)
  local Table           = tbltype:table()
  local tblname         = W._c_cache[Table].name
  return Table, tblname
end

local function live_ptr(self, storeptr, tbltype, rowsym)
  local Table, tblname  = unpack_tbl(self, tbltype)
  assert(Table._is_live, 'INTERNAL: expect is_live field')
  local livename        = self._c_cache[Table._is_live].name

  return `[&uint8](storeptr.[tblname].[livename] + rowsym)
end

local LIVE_MASK   = constant(uint8, 1)
local VISIT_MASK  = constant(uint8, 2)

function Wrapper:_INTERNAL_is_live_read(storeptr, tbltype, rowsym)
  return quote
    var ptr = [ live_ptr(self, storeptr, tbltype, rowsym) ]
    -- bitwise ops
  in [bool](@ptr and LIVE_MASK) end
end
function Wrapper:_INTERNAL_set_live(storeptr, tbltype, rowsym)
  return quote
    var ptr = [ live_ptr(self, storeptr, tbltype, rowsym) ]
    -- bitwise ops
    @ptr    = LIVE_MASK
  end
end

function Wrapper:_INTERNAL_set_dead(storeptr, tbltype, rowsym)
  return quote
    var ptr = [ live_ptr(self, storeptr, tbltype, rowsym) ]
    -- bitwise ops
    @ptr    = [uint8](0)
  end
end

function Wrapper:_INTERNAL_was_visited(storeptr, tbltype, rowsym)
  return quote
    var ptr = [ live_ptr(self, storeptr, tbltype, rowsym) ]
    -- bitwise ops
  in (@ptr and VISIT_MASK) ~= [uint8](0) end
end
function Wrapper:_INTERNAL_visit(storeptr, tbltype, rowsym)
  return quote
    var ptr = [ live_ptr(self, storeptr, tbltype, rowsym) ]
    -- bitwise ops
    @ptr    = @ptr or VISIT_MASK
  end
end
function Wrapper:_INTERNAL_clear_visit(storeptr, tbltype, rowsym)
  return quote
    var ptr = [ live_ptr(self, storeptr, tbltype, rowsym) ]
    -- bitwise ops
    @ptr    = @ptr and (not VISIT_MASK)
  end
end

function Wrapper:_INTERNAL_NewRow(storeptr, tbltype)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)
  local tbl_expr        = `storeptr.[tblname]

  -- more complicated if we have liveness and a free list to manage
  if Table._is_live then
    assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')
    local f0            = Table:primary_key()[1]
    local f0name        = W._c_cache[f0].name

    return quote
      var row             = tbl_expr._free_list
      if row == tbl_expr:n_alloc() then -- empty free list case
        tbl_expr:alloc_more(storeptr, tbl_expr:n_alloc() * 2)
      end
      tbl_expr._n_rows    = tbl_expr._n_rows + 1
      tbl_expr._is_sorted = false

      tbl_expr._free_list = (tbl_expr.[f0name])[row]
      [ W:_INTERNAL_set_live(storeptr, tbltype, row) ]
    in row end
  else
    return quote
      var row             = tbl_expr:size()
      tbl_expr:resize(storeptr, row+1)
    in row end
  end
end

function Wrapper:_INTERNAL_DeleteRow(storeptr, tbltype, rowval)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)
  local tbl_expr        = `storeptr.[tblname]

  assert(Table._is_live, 'INTERNAL: expect is_live field for deletions')
  assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')
  local f0              = Table:primary_key()[1]
  local f0name          = W._c_cache[f0].name

  return quote
    var row                   = [rowval]
    (tbl_expr.[f0name])[row]  = tbl_expr._free_list
    tbl_expr._free_list       = row
    [ W:_INTERNAL_set_dead(storeptr, tbltype, row) ]
    tbl_expr._n_rows          = tbl_expr._n_rows - 1
    tbl_expr._is_compact      = false -- not compact now
    tbl_expr._is_sorted       = false
  end
end

function Wrapper:_INTERNAL_ReserveRows(storeptr, tbltype, n_rows)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)
  local tbl_expr        = `storeptr.[tblname]

  assert(Table._is_live, 'INTERNAL: expect is_live field for deletions')
  assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')

  return quote
    tbl_expr:clear(storeptr)
    -- and get more space if needed...
    var N               = [n_rows]
    if N > tbl_expr:n_alloc() then
      tbl_expr:alloc_more(storeptr, N)
    end
    -- then turn on the first N rows
    tbl_expr._free_list = N
    for k=0,N do
      [ W:_INTERNAL_set_live(storeptr, tbltype, k) ]
    end
    tbl_expr._n_rows    = N
    -- this should result in a compact table
    tbl_expr._is_compact  = true
    tbl_expr._is_sorted   = false
  end
end

function Wrapper:_INTERNAL_EnsureCompact(storeptr, tbltype)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)
  assert(Table._is_live, 'INTERNAL: only call EnsureCompact on mergables')
  local tbl_expr        = `storeptr.[tblname]
  return quote
    if not tbl_expr:is_compact() then tbl_expr:compact(storeptr) end
  end
end

function Wrapper:_INTERNAL_EnsureSorted(storeptr, tbltype)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)
  assert(Table._is_live, 'INTERNAL: only call EnsureSorted on mergables')
  local tbl_expr        = `storeptr.[tblname]
  --local idx_print       = nil
  --for iIndex, Index in ipairs(W._indices) do
  --  local idxname       = W._c_cache[Index].name
  --  idx_print           = quote
  --    storeptr.[idxname]:debug_print(storeptr)
  --    storeptr.[idxname]:check_valid(storeptr)
  --  end
  --end
  return quote
    --[idx_print]
    [ W:_INTERNAL_EnsureCompact( storeptr, tbltype ) ]
    if not tbl_expr:is_sorted() then tbl_expr:sort(storeptr) end
    --[idx_print]
    --C.printf('endsort\n')
  end
end

-------------------------------------------------------------------------------
--[[                     Gong Internal Data Interface                      ]]--
-------------------------------------------------------------------------------

function Wrapper:Store_Struct()
  return self._c_cache[WRAPPER_ROOT].ctype
end
function Wrapper:has_GPU_support()
  return self._use_gpu
end
function Wrapper:GPU_Tables_Struct()
  return self._gpu_wrapper:GPU_Tables_Struct()
end
function Wrapper:GPU_Globals_Struct()
  return self._gpu_wrapper:GPU_Globals_Struct()
end
-- more pass-through to the gpu wrapper
for _,nm in ipairs({'LoopGen',
                    'Read','Write','Reduce',
                    'ReadGlobal','ReduceGlobal',
                    'Insert',
                    'PostEmit', 'PostMerge',
                    'MergeLookup', 'KeepRow', })
do
  Wrapper['GPU_'..nm] = function(self, ...)
    local fn = self._gpu_wrapper[nm]
    return fn(self._gpu_wrapper, ...)
  end
end

function Wrapper:Size(storeptr, tbltype)
  local Table, tblname  = unpack_tbl(self, tbltype)
  assert(not Table._is_live, 'INTERNAL: expect non-mergable table...')
  return (`storeptr.[tblname]:size())
end

function Wrapper:Scan(storeptr, tbltype, rowsym, bodycode)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)

  if Table._is_live then
    local guard         = W:_INTERNAL_is_live_read(storeptr, tbltype, rowsym)
    return quote
      var ALLOC         = storeptr.[tblname]._n_alloc
      var N_ROW         = storeptr.[tblname]._n_rows
      for [rowsym]=0,ALLOC do if [guard] then
        [bodycode]
        -- early exit
        N_ROW = N_ROW - 1
        if N_ROW == 0 then break end
      end end
    end
  else
    return quote
      var SIZE  = storeptr.[tblname]:size()
      for [rowsym]=0,SIZE do
        [bodycode]
      end
    end
  end
end
function Wrapper:SelfScan(storeptr, tbltype, row0sym, row1sym, bodycode)
  local Table, tblname  = unpack_tbl(self, tbltype)

  if Table._is_live then
    local guard0        = W:_INTERNAL_is_live_read(storeptr, tbltype, row0sym)
    local guard1        = W:_INTERNAL_is_live_read(storeptr, tbltype, row1sym)
    return quote
      var ALLOC         = storeptr.[tblname]:n_alloc()
      var N_ROW_0       = storeptr.[tblname]:n_rows()
      for [row0sym]=0,ALLOC do if [guard0] then
        var N_ROW_1     = storeptr.[tblname]:n_rows()
        for [row1sym]=row0sym,ALLOC do if [guard1] then
          [bodycode]
          -- early exit
          N_ROW_1 = N_ROW_1 - 1
          if N_ROW_1 == 0 then break end
        end end
        -- early exit
        N_ROW_0 = N_ROW_0 - 1
        if N_ROW_0 == 0 then break end
      end end
    end
  else
    return quote
      var SIZE  = storeptr.[tblname]:size()
      for [row0sym]=0,SIZE do
        for [row1sym]=row0sym,SIZE do
          [bodycode]
        end
      end
    end
  end
end

function Wrapper:DefaultDoubleScan(storeptr, tbl0, tbl1, row0, row1, body)
  local W               = self
  if tbl0 == tbl1 then
    return W:SelfScan(storeptr, tbl0, row0, row1, body)
  else
    return W:Scan(storeptr, tbl0, row0,
            W:Scan(storeptr, tbl1, row1,
              body))
  end
end

function Wrapper:LoopGen(storeptr, is_self_join,
                         traversal, row0sym, row1sym, args, bodycode)
  local W               = self
  local IndexL, IndexR  = traversal:left(), traversal:right()
  local inameL, inameR  = W._c_cache[IndexL].name, W._c_cache[IndexR].name
  local idxptrL         = `&(storeptr.[inameL])
  local idxptrR         = `&(storeptr.[inameR])

  local loopgen = traversal:_INTERNAL_LoopGen( self:GetAccAPI(), false )
  return loopgen( storeptr, is_self_join,
                  idxptrL, idxptrR, row0sym, row1sym, args, bodycode)
end

function Wrapper:Clear(storeptr, tbltype)
  local Table, tblname  = unpack_tbl(self, tbltype)
  local indices         = self._c_cache[Table].dst_indices

  if Table._is_live then
    assert(#indices == 1, 'INTERNAL: expect exactly 1 index')
    local Index         = indices[1]
    local iname         = self._c_cache[Index].name
    return quote
      storeptr.[tblname]:clear(storeptr)
      storeptr.[iname]:clear(storeptr)
    end
  else
    assert(#indices == 0, 'INTERNAL: expect no indices on non-deletable')
    return quote
      storeptr.[tblname]:resize(storeptr, 0)
    end
  end
end

local function apply_path(base, typ, path)
  local stmts = newlist()
  for _,tkn in ipairs(path) do
    if typ:is_record() then
      assert(is_id_str(tkn), 'INTERNAL: expect idstr')
      local expr        = base
      base              = `[expr].[tkn]
      local i,p         = typ:lookup(tkn)
      typ               = p.type
    elseif typ:is_tensor() then
      assert(terralib.islist(tkn), 'INTERNAL: expect list')
      assert(#tkn == #typ.dims, 'INTERNAL: bad num args to tensorindex')
      local expr        = base
      local offset      = `0
      local strides     = typ.rowstrides
      for i,a in ipairs(tkn) do
        local off       = offset
        offset          = `[off] + [ strides[i] ] * [a]
      end
      local oname       = symbol(offset:gettype())
      stmts:insert(quote var [oname] = [offset] end)
      base              = `[expr].d[oname]
      typ               = typ:basetype()
    else INTERNAL_ERR('unexpected field sub-type') end
  end
  return base, stmts, typ
end

local function prelude_lookup(self, storeptr, tbltype, rowsym, path)
  assert(#path >= 1 and is_id_str(path[1]),
         'INTERNAL: Must have a field to access')
  local Table, tblname  = unpack_tbl(self, tbltype)
  local Field           = assert(Table:fields(path[1]),
                                 'INTERNAL: field lookup failed')
  local fname           = self._c_cache[Field].name
  path                  = path:sublist(2)

  local fptr_expr       = `(storeptr.[tblname].[fname])[rowsym]
  local val, stmts, typ = apply_path(fptr_expr, Field:type(), path)

  return val, stmts, typ, Field
end

function Wrapper:Read(storeptr, tbltype, rowsym, path)
  local read_val, stmts     = prelude_lookup(self, storeptr,
                                             tbltype, rowsym, path)
  if #stmts == 0 then
    return read_val
  else
    return quote [stmts] in [read_val] end
  end
end

function Wrapper:Write(storeptr, tbltype, rowsym, path, rval)
  local lookup, stmts       = prelude_lookup(self, storeptr,
                                             tbltype, rowsym, path)

  return quote [stmts]; [lookup] = [rval] end
end

function Wrapper:Reduce(storeptr, tbltype, op, rowsym, path, rval)
  local lookup, stmts, typ  = prelude_lookup(self, storeptr,
                                             tbltype, rowsym, path)
  local REDUCE_OP           = CodeGen.REDUCE_OP

  if typ:is_tensor() then
    local btyp              = typ:basetype()
    local Nd                = typ._n_entries
    return quote
      [stmts]
      var rhs = [rval]
      for k=0,Nd do
        REDUCE_OP(op, lookup.d[k], rhs.d[k])
      end
    end
  else
    return quote [stmts]; REDUCE_OP(op, lookup, rval) end
  end
end

local function prelude_global(self, storeptr, Global, path)
  local globname            = self._c_cache[Global].name
  local globvar_expr        = `storeptr.[globname]
  local val, stmts, typ     = apply_path(globvar_expr, Global:type(), path)

  return val, stmts, typ, Global
end

function Wrapper:ReadGlobal(storeptr, glob, path)
  local read_val, stmts     = prelude_global(self, storeptr, glob, path)

  if #stmts == 0 then
    return read_val
  else
    return quote [stmts] in [read_val] end
  end
end

function Wrapper:WriteGlobal(storeptr, glob, path, rval)
  local gvar, stmts         = prelude_global(self, storeptr, glob, path)

  return quote [stmts]; [gvar] = [rval] end
end

function Wrapper:ReduceGlobal(storeptr, glob, op, path, rval)
  local gvar, stmts, typ    = prelude_global(self, storeptr, glob, path)
  local REDUCE_OP           = CodeGen.REDUCE_OP

  if typ:is_tensor() then
    local btyp              = typ:basetype()
    local Nd                = typ._n_entries
    return quote
      [stmts]
      var rhs = [rval]
      for k=0,Nd do
        REDUCE_OP(op, gvar.d[k], rhs.d[k])
      end
    end
  else
    return quote [stmts]; REDUCE_OP(op, gvar, rval) end
  end
end

function Wrapper:WriteCol(storeptr, tbltype, path, ptr, stride)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)

  assert(#path == 1 and type(path[1]) == 'string',
         'INTERNAL: field path expected')
  local Field           = assert(Table:fields(path[1]),
                                 'INTERNAL: field lookup failed')
  local fname           = self._c_cache[Field].name
  local ftype           = Field:type():terratype()

  return quote
    var SIZE    = [ ( Table._is_live and (`storeptr.[tblname]:n_rows())
                                      or (`storeptr.[tblname]:size()) ) ]
    var dst_ptr = storeptr.[tblname].[fname]
    var src_ptr = [&int8](ptr)
    var STRIDE  = stride
    for k=0,SIZE do
      dst_ptr[k] = @( [&ftype](src_ptr + k*STRIDE) )
    end
  end
end

function Wrapper:WriteRow(storeptr, tbltype, rowsym, vals)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)

  local stmts           = newlist()
  -- write values into the fields at that row
  for iField,Field in ipairs(Table:fields()) do
    local fname       = W._c_cache[Field].name
    local valexpr     = vals[iField]
    stmts:insert(quote (storeptr.[tblname].[fname])[rowsym] = [valexpr] end)
  end
  return quote [stmts] end
end

function Wrapper:Insert(storeptr, tbltype, vals)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)

  -- bind vals because we might use them twice
  local valsyms         = newlist()
  local valbinds        = newlist()
  for i,v in ipairs(vals) do
    if terralib.issymbol(v) then
      valsyms:insert(v)
    else
      local s             = symbol(v:gettype())
      valsyms:insert(s)
      valbinds:insert(quote var [s] = [v] end)
    end
  end

  -- get key values in case we need to do a lookup
  local dst_indices     = self._c_cache[Table].dst_indices
  local Index           = nil
  local k0, k1          = nil, nil
  if #dst_indices > 0 then
    assert(#dst_indices <= 1, 'INTERNAL: expect no more than one index')
    assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')
    local f0,f1         = unpack(Table:primary_key())
    local i0            = assert( tbltype:lookup(f0:name()) )
    local i1            = assert( tbltype:lookup(f1:name()) )
    k0, k1              = valsyms[i0], valsyms[i1]
    Index               = dst_indices[1]
  end

  return quote
    [valbinds]
    -- allocate a row for the insertion
    var row             = [ W:_INTERNAL_NewRow(storeptr, tbltype) ]
    -- and write values into the fields at that row
    [ W:WriteRow(storeptr, tbltype, row, valsyms) ]
    -- Then insert any needed entries in indices
    escape if Index then
      local iname       = self._c_cache[Index].name
      emit quote storeptr.[iname]:insert(k0, k1, row) end
    end end
  end --in row end -- might as well return the new row value
end


function Wrapper:PreMerge(storeptr, tbltype)
  -- nada under this design
  return quote end
end

function Wrapper:PostMerge(storeptr, tbltype, rm_var, rm_body)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)

  local dst_indices     = self._c_cache[Table].dst_indices
  assert(#dst_indices == 1, 'INTERNAL: expect exactly one index')
  assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')
  local Index           = dst_indices[1]
  local iname           = self._c_cache[Index].name
  local f0,f1           = unpack(Table:primary_key())
  local f0name          = self._c_cache[f0].name
  local f1name          = self._c_cache[f1].name

  -- run down the table, and clear out dead rows
  return quote
    var R_COUNT         = storeptr.[tblname]:n_rows()
    var ALLOC           = storeptr.[tblname]:n_alloc()
    for row=0,ALLOC do
      var liveval = [ live_ptr(W, storeptr, tbltype, row) ]
      if [ W:_INTERNAL_is_live_read(storeptr, tbltype, row) ] then
        var VISITED = [ W:_INTERNAL_was_visited(storeptr, tbltype, row) ]
        escape if rm_var then emit quote
          if not VISITED then
          -- run remove code, and check whether we decided to keep the row
            var [rm_var]  = row
            [rm_body]
            VISITED = [ W:_INTERNAL_was_visited(storeptr, tbltype, row) ]
          end
        end end end

        -- act based on final decision after possible remove clause
        if VISITED then
          [ W:_INTERNAL_clear_visit(storeptr, tbltype, row) ]
        else
          var key0      = (storeptr.[tblname].[f0name])[row]
          var key1      = (storeptr.[tblname].[f1name])[row]
          storeptr.[iname]:remove(key0, key1)
          [ W:_INTERNAL_DeleteRow(storeptr, tbltype, row) ]
        end
        -- early exit
        R_COUNT         = R_COUNT - 1
        if R_COUNT == 0 then break end
      end
    end
  end
end

function Wrapper:MergeLookup(
  storeptr, tbltype,
  row, key0, key1, body, else_vals
)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)

  local dst_indices     = self._c_cache[Table].dst_indices
  assert(#dst_indices == 1, 'INTERNAL: expect exactly one index')
  local Index           = dst_indices[1]
  local iname           = self._c_cache[Index].name

  return quote
    var success, rowptr = storeptr.[iname]:lookup([key0], [key1])
    if success then
      var [row]         = @rowptr
      [ W:_INTERNAL_visit(storeptr, tbltype, row) ]
      [body]
    else escape if else_vals then emit quote
      var row           = [ W:_INTERNAL_NewRow(storeptr, tbltype) ]
      @rowptr           = row -- complete index update
      [ W:_INTERNAL_visit(storeptr, tbltype, row) ]
      [ W:WriteRow(storeptr, tbltype, row, else_vals) ]
    end end end end
  end
end

function Wrapper:KeepRow(storeptr, tbltype, row)
  local W               = self
  return W:_INTERNAL_visit(storeptr, tbltype, row)
end

function Wrapper:Profile(storeptr, name, metric_type, ...)
  local W               = self
  return W._profiler:hook( `storeptr._profiler, name, metric_type, ... )
end

-------------------------------------------------------------------------------
--[[      Gong Internal Data Interface : Data Movement & Preparation       ]]--
-------------------------------------------------------------------------------
-- This function encapsulates data movement policy when mixing GPU
-- and CPU execution, which needs to be in one place in order to
-- reason about correctness of the policy.

local Effects       = require 'gong.src.effectcheck'
local is_scan       = Effects.Effects.Scan.check
local is_emit       = Effects.Effects.Emit.check
local is_merge      = Effects.Effects.Merge.check

local is_overwrite  = Effects.Effects.OverWrite.check
local is_write      = Effects.Effects.Write.check
local is_reduce     = Effects.Effects.Reduce.check
local is_read       = Effects.Effects.Read.check

local is_reduce_global  = Effects.Effects.ReduceG.check
local is_read_global    = Effects.Effects.ReadG.check

function Wrapper:AccIndexUpdate( traversal, name, storeptr,
                                 gpu_tblptr, gpu_globptr )

  local L_AIndex    = traversal:left()
  local R_AIndex    = traversal:right()
  local lname       = self._c_cache[L_AIndex].name
  local rname       = self._c_cache[R_AIndex].name
  local lidxptr     = (`storeptr.[lname])
  local ridxptr     = (`storeptr.[rname])

  return traversal:_INTERNAL_PreJoinUpdate( self:GetAccAPI(),
                                            self:GetAccAPI(),
                                            name, storeptr, lidxptr, ridxptr,
                                            gpu_tblptr, gpu_globptr )
  --local l_update = L_AIndex:_INTERNAL_PreJoinUpdate(
  --                      self:GetAccAPI(), name, storeptr, lidxptr,
  --                                              gpu_tblptr, gpu_globptr )
  --if L_AIndex == R_AIndex then
  --  return l_update
  --else
  --  local r_update = R_AIndex:_INTERNAL_PreJoinUpdate(
  --                        self:GetAccAPI(), name, storeptr, ridxptr,
  --                                                gpu_tblptr, gpu_globptr )
  --  return quote [l_update] ; [r_update] end
  --end
end

function Wrapper:AccIndexInvalidate_TableSize(storeptr, Table)
  local code = newlist()
  for iAIndex,AIndex in ipairs(self._acc_indices) do
    if AIndex:_INTERNAL_depends_on(Table) then
      local aname   = self._c_cache[AIndex].name
      local idxptr  = (`storeptr.[aname])
      code:insert(AIndex:_INTERNAL_do_invalidate( idxptr, true, true ))
    end
  end
  return quote [code] end
end

function Wrapper:AccIndexInvalidate_FieldWrite(storeptr, Field)
  local code = newlist()
  for iAIndex,AIndex in ipairs(self._acc_indices) do
    if AIndex:_INTERNAL_depends_on(Field) then
      local aname   = self._c_cache[AIndex].name
      local idxptr  = (`storeptr.[aname])
      code:insert(AIndex:_INTERNAL_do_invalidate( idxptr, false, true ))
    end
  end
  return quote [code] end
end

function Wrapper:AccIndexInvalidate_GlobalWrite(storeptr, Global)
  local code = newlist()
  for iAIndex,AIndex in ipairs(self._acc_indices) do
    if AIndex:_INTERNAL_depends_on(Global) then
      local aname   = self._c_cache[AIndex].name
      local idxptr  = (`storeptr.[aname])
      code:insert(AIndex:_INTERNAL_do_invalidate( idxptr, false, true ))
    end
  end
  return quote [code] end
end


function Wrapper:AccIndexInvalidation(storeptr, effects)
  local accidx      = {}
  for iAIndex,AIndex in ipairs(self._acc_indices) do
    accidx[AIndex]  = {
      size_invalid  = false,
      data_invalid  = false,
    }
  end

  -- build up accounting of whether and how indices need to be invalidated
  for i,e in ipairs(effects) do
    if is_emit(e) or is_merge(e) then
      local tbl   = e.dst:table()
      for AIndex,bits in pairs(accidx) do
        if AIndex:_INTERNAL_depends_on(tbl) then
          bits.size_invalid = true
          bits.data_invalid = true
        end
      end
    elseif is_write(e) or is_overwrite(e) or is_reduce(e) then
      local tbl   = e.dst:table()
      local fld   = tbl:fields(e.path[1].name)
      for AIndex,bits in pairs(accidx) do
        if AIndex:_INTERNAL_depends_on(e.dst) then
          bits.data_invalid = true
        end
      end
    elseif is_reduce_global(e) then
      local glob  = e.dst
      for AIndex,bits in pairs(accidx) do
        if AIndex:_INTERNAL_depends_on(glob) then
          bits.data_invalid = true
        end
      end
    end
  end

  local code = newlist()
  for AIndex,bits in pairs(accidx) do
    local aname   = self._c_cache[AIndex].name
    local idxptr  = (`storeptr.[aname])
    code:insert(AIndex:_INTERNAL_do_invalidate( idxptr,
                                                bits.size_invalid,
                                                bits.data_invalid ))
  end

  return quote [code] end
end

function Wrapper:PrepareData(storeptr, for_gpu, effects)
  local W = self

  -- tables to put in emit or merge-mode
  local emittbls        = {}
  local mergetbls       = {}
  -- whether to ensure global validity
  local r_globals       = false -- true if rw_globals is true
  local rw_globals      = false
  for i,e in ipairs(effects) do
    if     is_emit(e)          then emittbls[e.dst] = true 
    elseif is_merge(e)         then mergetbls[e.dst] = true
    elseif is_reduce_global(e) then rw_globals  = true
                                    r_globals   = true
    elseif is_read_global(e)   then r_globals   = true
    end
  end

  -- fields
  local read_tbls       = {}
  local overwrite_tbls  = {}
  local readwrite_tbls  = {}
  for i,e in ipairs(effects) do
    if emittbls[e.src] or emittbls[e.dst] or
       mergetbls[e.src] or mergetbls[e.dst] then -- no-op

    elseif is_read(e) then
      local readcache       = read_tbls[e.src] or {}
      read_tbls[e.src]      = readcache
      -- for now, just focus on the first path element...
      local fld = e.path[1].name
      readcache[fld]        = true

    elseif is_reduce(e) or is_write(e) then
      local writecache      = readwrite_tbls[e.src] or {}
      readwrite_tbls[e.dst] = writecache
      -- for now, just focus on the first path element...
      local fld = e.path[1].name
      writecache[fld]       = true

    elseif is_overwrite(e) then
      local owcache         = overwrite_tbls[e.dst]
      overwrite_tbls[e.dst] = owcache
      -- for now, just focus on the first path element...
      local fld = e.path[1].name
      owcache[fld]          = true
    end
  end


  -- CPU ONLY data preparation is much simpler...
  if not W._use_gpu then
    local code = newlist()

    -- clear out tables we're going to emit into
    for dst,_ in pairs(emittbls)  do
      code:insert( self:Clear(storeptr, dst) )     end
    -- prepare any merge tables
    for dst,_ in pairs(mergetbls) do
      code:insert( self:PreMerge(storeptr, dst) )  end

    return quote [code] end


  -- GPU ENABLED data preparation has to do a lot more
  -- because of data movement and validity tracking
  else
    local code  = newlist()
    local GW    = W._gpu_wrapper

    -- moving the globals
    if rw_globals then
      code:insert( GW:PrepareReadWriteGlobals(storeptr, for_gpu) )
    elseif r_globals then
      code:insert( GW:PrepareReadGlobals(storeptr, for_gpu) )
    end

    -- First, consider an operation with no emits or merges.
    --    This operation must move all data that is being read or
    --    written to the correct processor.
    --    Then, all of the data that is being written needs to be
    --    invalidated everywhere else.
    -- Second, consider an operation with emits
    --    This operation must additionally clear the table
    --    and overwrite the data, including potential re-allocations
    --  **  These operations must invalidate the metadata  **

    for dst,cache in pairs(overwrite_tbls) do
      for fld,_ in pairs(cache) do
        code:insert( GW:PrepareOverWrite(storeptr, dst, fld, for_gpu) )
    end end
    for dst,cache in pairs(readwrite_tbls) do
      for fld,_ in pairs(cache) do
        code:insert( GW:PrepareReadWrite(storeptr, dst, fld, for_gpu) )
    end end
    for src,cache in pairs(read_tbls) do
      for fld,_ in pairs(cache) do
        -- additionally guard against redundant code insertion for
        -- fields already being read and written
        if not readwrite_tbls[src] or not readwrite_tbls[src][fld] then
          code:insert( GW:PrepareRead(storeptr, src, fld, for_gpu) )
    end end end

    -- clear out tables we're going to emit into
    for dst,_ in pairs(emittbls)  do
      code:insert( W._gpu_wrapper:Clear(storeptr, dst, for_gpu) )
    end
    -- if we're going to merge into a table, then
    -- prepare the merge...
    for dst,_ in pairs(mergetbls) do
      code:insert( W._gpu_wrapper:PreMerge(storeptr, dst, for_gpu) )
    end

    -- if we are clearing any tables on the gpu,
    -- then we need different finalization behavior...
    local gpu_size_modify = for_gpu and (next(emittbls) ~= nil or
                                         next(mergetbls) ~= nil)
    code:insert( W._gpu_wrapper:PrepareFinal(storeptr, for_gpu,
                                             gpu_size_modify) )

    return quote [code] end
  end
end


-------------------------------------------------------------------------------
--[[         Gong Internal Data Interface : Acceleration Structures        ]]--
-------------------------------------------------------------------------------

function Wrapper:GetAccAPI()
  local W             = self
  if W._cached_acc_API then return W._cached_acc_API end
  W._cached_acc_API   = {
    StoreTyp          = function(api) return W:Store_Struct() end,
    GPU_TablesTyp     = function(api) return W:GPU_Tables_Struct() end,
    GPU_GlobalsTyp    = function(api) return W:GPU_Globals_Struct() end,
    HasGPUSupport     = function(api) return W:has_GPU_support() end,
    Size              = function(api, storeptr, tbltype)
                          if W:has_GPU_support() then
                            return quote
                              [W._gpu_wrapper:CPU_SizeRefresh( storeptr,
                                                               tbltype )]
                            in [W:Size(storeptr, tbltype)] end
                          else
                            return W:Size(storeptr, tbltype)
                          end
                        end,
    IndexPtr          = function(api, storeptr, SpatialIndex)
                          local iname = W._c_cache[SpatialIndex].name
                          return `&([storeptr].[iname])
                        end,
    Scan              = function(api, ...)
                          return W:Scan(...)
                        end,
    SelfScan          = function(api, ...)
                          return W:SelfScan(...)
                        end,
    GPU_Scan          = function(api, ...)
                          return W._gpu_wrapper:Scan(...)
                        end,
    GPU_DoubleScan    = function(api, ...)
                          return W._gpu_wrapper:ScanAB(...)
                        end,
    GPU_SelfScan      = function(api, ...)
                          return W._gpu_wrapper:ScanAA(...)
                        end,
    GetCPUFunction    = function(api, gongfn)
                          return W:get_terra_function(gongfn, false)
                        end,
    GetGPUFunction    = function(api, gongfn)
                          return W:get_terra_function(gongfn, true)
                        end,
    Profile           = function(api, ...)
                          return W:Profile(...)
                        end,
  }
  return W._cached_acc_API
end

-------------------------------------------------------------------------------
--[[                     Gong External Data Interface                      ]]--
-------------------------------------------------------------------------------

function Wrapper:GenExternCAPI(prefix, export_funcs, gpu_on)
  local W             = self
  local GW            = W._gpu_wrapper
  prefix              = (prefix and prefix..'_') or ''

  local STRUCTS       = newlist()
  local FUNCS         = newlist()
  local HIERARCHY     = {}
  local function add_func(name, tbl, key, f)
    if f then
      tbl[key]        = f
    else
      f               = tbl
    end
    f:setname(name)
    FUNCS:insert(f)
  end
  local function add_func_note(str)
    FUNCS:insert(str)
  end

  local CStore    = W._c_cache[WRAPPER_ROOT].ctype

  -- Create Store Handle
  local ExtStore  = terralib.types.newstruct(prefix..'Store')
  ExtStore.entries:insert{ 'id', uint64 }
  STRUCTS:insert(ExtStore)
  local to_store  = macro(function(hdl)
    assert(hdl:gettype() == ExtStore)
    return `[&CStore](hdl.id)
  end)
  local to_hdl    = macro(function(store)
    assert(store:gettype() == &CStore)
    return `[ExtStore]({ [uint64](store) })
  end)

  HIERARCHY.Store           = ExtStore


  -- Exported Joins & Functions

  add_func_note("/* Exported Joins */")
  HIERARCHY.joins = {}
  for _,jf in ipairs(export_funcs) do
    local tfunc   = W:get_terra_function(jf, false)
    if verbosity > 3 then tfunc:printpretty(false) end
    local args    = newlist()
    local params  = tfunc:gettype().parameters
    for i=2,#params do
      args:insert( symbol(params[i]) )
    end

    add_func(jf:getname(), HIERARCHY.joins, jf:getname(),
    terra( hdl : ExtStore, [args] )
      [ W:Profile( `to_store(hdl), jf:getname()..'_launches', 'timer_start' )]
      tfunc( to_store(hdl), [args] )
      [ W:Profile( `to_store(hdl), jf:getname()..'_launches', 'timer_stop' )]
    end)
  end
  add_func_note("")
  if gpu_on then
    add_func_note("/* Exported Joins (GPU versions) */")
    for _,jf in ipairs(export_funcs) do
      local tfunc   = W:get_terra_function(jf, true)
      if verbosity > 3 then tfunc:printpretty(false) end
      local args    = newlist()
      local params  = tfunc:gettype().parameters
      for i=2,#params do
        args:insert( symbol(params[i]) )
      end

      local name = gpu_name(jf)
      add_func(name, HIERARCHY.joins, name,
      terra( hdl : ExtStore, [args] )
        [ W:Profile( `to_store(hdl),
                     jf:getname()..'_GPU_launches', 'timer_start' )]
        tfunc( to_store(hdl), [args] )
        [ W:Profile( `to_store(hdl),
                     jf:getname()..'_GPU_launches', 'timer_stop' )]
      end)
    end
    add_func_note("")
  end


  -- extra initialization patch in

  local init_CUDA = quote end
  if gpu_on then
    local terra do_cuda_init()
      escape for _,initfn in ipairs(GW._cuda_loaders) do emit quote
        --C.printf("INIT %s\n", [initfn:getname()])
        initfn()
      end end end
    end
    init_CUDA = quote do_cuda_init() end
  end

  -- NewStore, DestroyStore, GetError

  add_func_note("/* New and Destroy Store; Error Handling */")
  add_func('NewStore', HIERARCHY, 'NewStore',
  terra() : ExtStore
    [init_CUDA]
    var store     = alloc_ptr(CStore, 1)
    store:init()
    return to_hdl(store)
  end)
  add_func('DestroyStore', HIERARCHY, 'DestroyStore',
  terra( hdl : ExtStore )
    to_store(hdl):destroy()
  end)
  add_func('GetError', HIERARCHY, 'GetError',
  terra( hdl : ExtStore ) : rawstring
    return to_store(hdl):get_error()
  end)
  add_func_note("")

  -- Profiling

  add_func_note("/* Profiling Output */")
  add_func('PrintProfile', HIERARCHY, 'PrintProfile',
  terra( hdl : ExtStore )
    [ W._profiler:print_profile( `to_store(hdl)._profiler ) ]
  end)

  HIERARCHY.Profile           = W._profiler:get_output_struct()
  STRUCTS:insert(HIERARCHY.Profile)
  add_func('GetProfile', HIERARCHY, 'GetProfile',
  terra( hdl : ExtStore ) : HIERARCHY.Profile
    return [ W._profiler:get_output( `to_store(hdl)._profiler ) ]
  end)
  add_func_note("")



  -- Table Data Access

  HIERARCHY.tables = {}
  for iTable,Table in ipairs(W._tables) do
    local tname               = Table:name()
    local tbl_cname           = W._c_cache[Table].name
    local SizeT               = Table:_INTERNAL_terra_size_type()
    local KeyT                = Table:_INTERNAL_terra_key_type()

    HIERARCHY.tables[tname]   = {}

    -- Build Schema Note
    add_func_note("/*")
    add_func_note("Table "..tname.." {")
    for iField,Field in ipairs(Table:fields()) do
      local fname           = Field:name()
      local ftype           = Field:type():terratype()
      add_func_note("  "..fname.." : "..tostring(ftype))
    end
    if Table._is_live then
      add_func_note("  // the following field is read-only")
      add_func_note("  is_live : bool")
    end
    add_func_note("}")
    add_func_note("*/")

    -- Check Table Size
    if Table._is_live then
      add_func('GetNRows_'..tname, HIERARCHY.tables[tname], 'GetNRows',
      terra( hdl : ExtStore ) : SizeT
        [ (gpu_on and GW:CPU_SizeRefresh(`to_store(hdl), T.row(Table)))
                  or {} ]
        return to_store(hdl).[tbl_cname]:n_rows()
      end)
      add_func('GetNAlloc_'..tname, HIERARCHY.tables[tname], 'GetNAlloc',
      terra( hdl : ExtStore ) : SizeT
        [ (gpu_on and GW:CPU_SizeRefresh(`to_store(hdl), T.row(Table)))
                  or {} ]
        return to_store(hdl).[tbl_cname]:n_alloc()
      end)
      add_func('Sort_'..tname, HIERARCHY.tables[tname], 'Sort',
      terra( hdl : ExtStore )
        var store = to_store(hdl)
        [ (gpu_on and GW:CPU_SizeRefresh(`to_store(hdl), T.row(Table)))
                  or {} ]
        [ W:_INTERNAL_EnsureSorted(store, T.row(Table)) ]
      end)
    else
      add_func('GetSize_'..tname, HIERARCHY.tables[tname], 'GetSize',
      terra( hdl : ExtStore ) : SizeT
        [ (gpu_on and GW:CPU_SizeRefresh(`to_store(hdl), T.row(Table)))
                  or {} ]
        return to_store(hdl).[tbl_cname]:size()
      end)
    end

    -- Bulk Load
    add_func('BeginLoad_'..tname, HIERARCHY.tables[tname], 'BeginLoad',
    terra( hdl : ExtStore, size : SizeT )
      var store = to_store(hdl)
      if store._load_counter >= 0 or store._load_col_counter >= 0 then
        store:error('cannot begin load while another table is loading')
      end
      store._load_counter     = 0
      store._load_col_counter = 0
      [ (gpu_on and GW:CPU_PrepareLoadTable(store, T.row(Table)) )
                or {} ]
      escape if Table._is_live then
        emit( W:_INTERNAL_ReserveRows(store, T.row(Table), size) )
      else emit quote
        store.[tbl_cname]:resize(store, size)
      end end end
    end)
    add_func('EndLoad_'..tname, HIERARCHY.tables[tname], 'EndLoad',
    terra( hdl : ExtStore )
      var store         = to_store(hdl)
      var size          = [ (Table._is_live and `store.[tbl_cname]:n_rows())
                                             or `store.[tbl_cname]:size() ]
      var row_success   = ( store._load_counter < size )
      var col_success   = ( store._load_col_counter < [#Table:fields()] )
      if not row_success and not col_success then
        store:error(
          ['expected to see %d rows loaded or %d columns loaded,\n'..
           'but only %d rows and %d columns have been loaded so far\n'],
          size,                [#Table:fields()],
          store._load_counter, store._load_col_counter)
      end
      store._load_counter = -1
      store._load_col_counter = -1

      [ W:AccIndexInvalidate_TableSize( store, Table ) ]

      -- refresh indices if appropriate
      escape if Table._is_live then
        local indices   = W._c_cache[Table].dst_indices
        assert(#indices == 1, 'INTERNAL: expect exactly 1 index')
        local Index     = indices[1]
        local iname     = W._c_cache[Index].name
        emit quote  store.[iname]:rebuild(store)  end
        if gpu_on then
          emit( GW:CPU_index_rebuild(store, Index) )
        end
      end end
    end)

    local vals = newlist()
    for iField,Field in ipairs(Table:fields()) do
      local ftype             = Field:type():terratype()
      vals:insert( symbol(ftype, 'arg'..(iField-1)) )
    end
    add_func('LoadRow_'..tname, HIERARCHY.tables[tname], 'LoadRow',
    terra( hdl : ExtStore, [vals] )
      var store = to_store(hdl)
      var row   = store._load_counter
      store._load_counter = row + 1
      var size  = [ (Table._is_live and `store.[tbl_cname]:n_rows())
                                     or `store.[tbl_cname]:size() ]
      if row >= size then
        store:error(['tried to load more than %d rows into table '..tname],
                    size)
      else
        [ W:WriteRow(store, T.row(Table), row, vals) ]
      end
    end)

    HIERARCHY.tables[tname].fields  = {}
    local H_FIELDS                  = HIERARCHY.tables[tname].fields
    for iField,Field in ipairs(Table:_INTERNAL_fields()) do
      local fname           = Field:name()
      local f_cname         = W._c_cache[Field].name
      local ftype           = Field:type():terratype()

      H_FIELDS[fname]       = {}
      local IS_LIVE         = (fname == 'is_live')
      local IS_IDX_KEY      = false
      local iname           = nil
      local Index           = nil
      if Table._is_live then
        local f0,f1         = unpack(Table:primary_key())
        IS_IDX_KEY          = (Field == f0  or  Field == f1)

        local indices       = W._c_cache[Table].dst_indices
        assert(#indices == 1, 'INTERNAL: expect exactly 1 index')
        Index               = indices[1]
        iname               = W._c_cache[Index].name
      end

      -- Per-Row Point Access

      if IS_LIVE then
        -- Per-Row Point Access
        add_func('Read_'..tname..'_is_live', H_FIELDS['is_live'], 'Read',
        terra( hdl : ExtStore, row : KeyT ) : ftype
          var store = to_store(hdl)
          [ (true and (`assert(false,'INTERNAL: simple Read deprecated')))
                    or {} ]
          return [ W:_INTERNAL_is_live_read(store, T.row(Table), row) ]
        end)
      else
        -- Column Loads
        add_func('Load_'..tname..'_'..fname, H_FIELDS[fname], 'Load',
        terra( hdl : ExtStore, ptr : &ftype, stride : uint32 )
          var store = to_store(hdl)
          return [ W:WriteCol(store, T.row(Table), newlist{fname},
                              ptr, stride ) ]
        end)

        -- Per-Row Point Access
        add_func('Read_'..tname..'_'..fname, H_FIELDS[fname], 'Read',
        terra( hdl : ExtStore, row : KeyT ) : ftype
          var store = to_store(hdl)
          [ (true and (`assert(false,'INTERNAL: simple Read deprecated')))
                    or {} ]
          return [ W:Read(store, T.row(Table), row, newlist{fname} ) ]
        end)
        add_func('Write_'..tname..'_'..fname, H_FIELDS[fname], 'Write',
        terra( hdl : ExtStore, row : KeyT, val : ftype )
          var store = to_store(hdl)
          [ (true and (`assert(false,'INTERNAL: simple Write deprecated')))
                    or {} ]
          return [ W:Write(store, T.row(Table), row, newlist{fname}, val ) ]
        end)

        -- Simple bulk access
        add_func('ReadWriteLock_'..tname..'_'..fname,
                 H_FIELDS[fname], 'ReadWriteLock',
        terra( hdl : ExtStore ) : &ftype
          var store = to_store(hdl)
          do [ (gpu_on and GW:PrepareReadWrite(store, T.row(Table),
                                               fname, false) ) or {} ] end
          escape if IS_IDX_KEY then emit quote
            store.[tbl_cname]._is_sorted = false
          end end end
          return store.[tbl_cname].[f_cname]
        end)
        add_func('ReadWriteUnLock_'..tname..'_'..fname,
                 H_FIELDS[fname], 'ReadWriteUnlock',
        terra( hdl : ExtStore )
          var store = to_store(hdl)
          [ W:AccIndexInvalidate_FieldWrite( store, Field ) ]
          escape if IS_IDX_KEY then emit quote
            store.[iname]:rebuild(store)
            [ (gpu_on and GW:CPU_index_rebuild(store, Index)) or {} ]
          end end end
        end)
      end

      -- Simple bulk access
      add_func('ReadLock_'..tname..'_'..fname,
               H_FIELDS[fname], 'ReadLock',
      terra( hdl : ExtStore ) : &ftype
        var store = to_store(hdl)
        do [ (gpu_on and GW:PrepareRead(store, T.row(Table),
                                        fname, false) ) or {} ] end
        return store.[tbl_cname].[f_cname]
      end)
      add_func('ReadUnLock_'..tname..'_'..fname,
               H_FIELDS[fname], 'ReadUnlock',
      terra( hdl : ExtStore )
        -- no-op
      end)

    end
    add_func_note("")
  end

  -- Global Data Access

  HIERARCHY.globals = {}
  for iGlobal,Global in ipairs(W._globals) do
    local gname               = Global:name()
    local gbl_cname           = W._c_cache[Global].name
    local gtype               = Global:type():terratype()

    HIERARCHY.globals[gname]  = {}

    -- Build Schema Note
    add_func_note("/*")
    add_func_note("Global "..gname.." : "..tostring(gtype))
    add_func_note("*/")

    -- Read/Write access to the global
    add_func('Read_'..gname, HIERARCHY.globals[gname], 'Read',
    terra( hdl : ExtStore ) : gtype
      var store = to_store(hdl)
      [ (gpu_on and GW:PrepareReadGlobals(store, false))
                or {} ]
      return [ W:ReadGlobal( store, Global, newlist() ) ]
    end)
    add_func('Write_'..gname, HIERARCHY.globals[gname], 'Write',
    terra( hdl : ExtStore, val : gtype )
      var store = to_store(hdl)
      [ W:AccIndexInvalidate_GlobalWrite( store, Global ) ]
      [ (gpu_on and GW:PrepareReadWriteGlobals(store, false))
                or {} ]
      return [ W:WriteGlobal( store, Global, newlist(), val ) ]
    end)
    add_func_note("")
  end

  return FUNCS, STRUCTS, HIERARCHY
end





