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

local StdContainers = require 'gong.src.stdcontainers'
local vector        = StdContainers.vector

local CodeGen       = require 'gong.src.codegen'

local C             = require 'gong.src.c'
local assert        = C.assert

local newlist       = terralib.newlist

local verbosity = require('gong.src.verbosity').get_verbosity()

-------------------------------------------------------------------------------

--[[

  Sketch of Current Storage Policy.

    This sketch is provided because tracing through a bunch of code
    that metaprograms data-structure layout from the schematic description
    is a brain-melting activity.  Really, we just need a clear picture
    of what we're trying to do, and then to do it.
  
  Here is a grammatical summary of the abstract data model
    <Schema>  ::= (<Table> `;`)*
    <Table>   ::= <name> `{` <Field>* `}`
    <Field>   ::= <name> `:` <Type>
    <Index>   ::= <name> `:` <Field>+ `->` <Table>
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

  Specifically, a CStore, and CTable look like the following

    CStore {
      <table_name_0>  : CTable_0
      <table_name_1>  : CTable_1
      ...
    }
    
    CTable_i {
      alloc_size      : Size_Type_i     -- allocated space
      size            : Size_Type_i     -- used space
      <col_name_0>    : &<type_0>
      <col_name_1>    : &<type_1>
      ...
      // IF SETUP FOR MERGING/PRIMARY KEY
      is_live         : &bool
    }
    If this table is setup for individual row deletion too, then...
    CTable_i {
      n_alloc         : Size_Type_i
      n_row           : Size_Type_i
      free_list       : Key_Type_i
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
      var lo,hi   = 0,int32(N)-1
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
      assert(lo == hi + 1, 'INTERNAL: sanity 1 for index find')
      assert(lo >= 0, 'INTERNAL: sanity 2 for index find')
      return false, v, lo
    end

    local terra print_idx( idx : CIdx, k0 : K0 )
      C.printf("  idx[%d]\n", k0)
      var V = idx._keys[k0]
      for k=0,V:size() do
        C.printf("    %d: %d %d\n", k, V(k).k1, V(k).dst)
      end
    end

    terra CIdx:insert( k0 : K0, k1 : K1, row : KDST )
      var success, v, i = find(self, k0, k1)
      assert(not success, "INTERNAL: expect insert-entry to be missing")
      var N             = v:size()
      v:resize(N+1)
      -- shift contents and then enter data
      for k=i,N do v(k+1) = v(k) end
      v(i).k1           = k1
      v(i).dst          = row
    end
    terra CIdx:remove( k0 : K0, k1 : K1 )
      var success, v, i = find(self, k0, k1)
      assert(success, "INTERNAL: expect remove-entry to be present")
      var N             = v:size()
      -- shift contents right
      for k=i,N-1 do v(k) = v(k+1) end
      v:resize(N-1)
    end
    terra CIdx:lookup( k0 : K0, k1 : K1 )
      var success, v, i = find(self, k0, k1)
      --print_idx(@self, k0)
      if success then
        return true, &(v(i).dst)
      else
        var N             = v:size()
        v:resize(N+1)
        -- shift contents and then enter data
        for k=i,N do v(k+1) = v(k) end
        v(i).k1           = k1
        return false, &(v(i).dst)
      end
    end

    terra CIdx:rebuild( store : &CStore )
      self:clear(store)
      var ALLOC           = store.[Dname]._n_alloc
      for row=0,ALLOC do
        if [ W:_INTERNAL_is_live_read(store, T.row(DST), row) ] then
          var k0          = (store.[Dname].[F0name])[row]
          var k1          = (store.[Dname].[F1name])[row]
          self:insert(k0, k1, row)
      end end
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
                                dst_indices       = newlist(), }
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

  CStore.entries:insertall {
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
    local SizeT             = Table:_INTERNAL_terra_size_type()
    local MAXsize           = Table:_INTERNAL_terra_MAX_SIZE()
    local indices           = W._c_cache[Table].parallel_indices

    if Table._is_live then
      CTable.methods.n_rows = macro(function(self) return `self._n_rows end)
      CTable.methods.n_alloc = macro(function(self) return `self._n_alloc end)

      assert(#indices == 0, 'INTERNAL: expect no indices from is_live table')
      local f0              = Table:primary_key()[1]
      local f0name          = W._c_cache[f0].name

      terra CTable:alloc_more( store : &CStore, newalloc : SizeT )
        var this              = self
        var oldalloc          = self._n_alloc
        self._n_alloc         = newalloc
        assert(newalloc > oldalloc, 'INTERNAL: no need to alloc_more')
        assert(self._free_list == oldalloc,
               'INTERNAL: expected empty free-list')

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
      end
    end for iIndex, Index in ipairs(W._indices) do
      local idxname           = W._c_cache[Index].name
      emit quote
        this.[idxname]:init( this )
      end
    end end
  end
  terra CStore:destroy()
    var this        = self
    this._error_msg = nil
    escape for iIndex, Index in ipairs(W._indices) do
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
    end end
  end
end

local function NewWrapper(prefix, schema_tables, schema_indices)
  prefix = (prefix and prefix..'_') or ''

  local W = setmetatable({
    _tables         = schema_tables:copy(),
    _indices        = schema_indices:copy(),
    _c_cache        = {},
    _type_reverse   = {},
    _func_cache     = {},
  }, Wrapper)

  GenerateWrapperStructs(prefix, W)
  InstallStructMethods(prefix, W)

  return W
end
Exports.GenerateDataWrapper = NewWrapper


-------------------------------------------------------------------------------
--[[                       Code Generation Interface                       ]]--
-------------------------------------------------------------------------------

function Wrapper:get_terra_function(f_obj)
  if not self._func_cache[f_obj] then
    local tfunc             = CodeGen.codegen(f_obj:getname(),
                                              f_obj:_INTERNAL_getast(),
                                              f_obj:_INTERNAL_geteffects(),
                                              self)
    self._func_cache[f_obj] = tfunc
  end
  return self._func_cache[f_obj]
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
      var row           = tbl_expr._free_list
      if row == tbl_expr:n_alloc() then -- empty free list case
        tbl_expr:alloc_more(storeptr, tbl_expr:n_alloc() * 2)
      end
      tbl_expr._n_rows  = tbl_expr._n_rows + 1

      tbl_expr._free_list = (tbl_expr.[f0name])[row]
      [ W:_INTERNAL_set_live(storeptr, tbltype, row) ]
    in row end
  else
    return quote
      var row           = tbl_expr:size()
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
  end
end

function Wrapper:_INTERNAL_ReserveRows(storeptr, tbltype, n_rows)
  local W               = self
  local Table, tblname  = unpack_tbl(self, tbltype)
  local tbl_expr        = `storeptr.[tblname]

  assert(Table._is_live, 'INTERNAL: expect is_live field for deletions')
  assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')
  local f0              = Table:primary_key()[1]
  local f0name          = W._c_cache[f0].name

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
  end
end

-------------------------------------------------------------------------------
--[[                     Gong Internal Data Interface                      ]]--
-------------------------------------------------------------------------------

function Wrapper:Store_Struct()
  return self._c_cache[WRAPPER_ROOT].ctype
end

function Wrapper:Scan(storeptr, tbltype, rowsym, bodycode)
  local Table, tblname  = unpack_tbl(self, tbltype)

  if Table._is_live then
    local guard         = W:_INTERNAL_is_live_read(storeptr, tbltype, rowsym)
    return quote
      var ALLOC         = storeptr.[tblname]:n_alloc()
      var N_ROW         = storeptr.[tblname]:n_rows()
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

function Wrapper:DoubleScan(storeptr, tbltype, row0sym, row1sym, bodycode)
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

function Wrapper:PostMerge(storeptr, tbltype)
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
        if [ W:_INTERNAL_was_visited(storeptr, tbltype, row) ] then
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

-------------------------------------------------------------------------------
--[[                     Gong External Data Interface                      ]]--
-------------------------------------------------------------------------------

function Wrapper:GenExternCAPI(prefix, export_funcs)
  local W             = self
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

  -- NewStore, DestroyStore, GetError

  add_func_note("/* New and Destroy Store; Error Handling */")
  add_func('NewStore', HIERARCHY, 'NewStore',
  terra() : ExtStore
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

  -- Exported Joins & Functions

  add_func_note("/* Exported Joins */")
  HIERARCHY.joins = {}
  for _,jf in ipairs(export_funcs) do
    local tfunc   = W:get_terra_function(jf)
    if verbosity > 3 then tfunc:printpretty(false) end
    local args    = newlist()
    local params  = tfunc:gettype().parameters
    for i=2,#params do
      args:insert( symbol(params[i]) )
    end

    add_func(jf:getname(), HIERARCHY.joins, jf:getname(),
    terra( hdl : ExtStore, [args] )
      return tfunc( to_store(hdl), [args] )
    end)
  end
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
        return to_store(hdl).[tbl_cname]:n_rows()
      end)
      add_func('GetNAlloc_'..tname, HIERARCHY.tables[tname], 'GetNAlloc',
      terra( hdl : ExtStore ) : SizeT
        return to_store(hdl).[tbl_cname]:n_alloc()
      end)
    else
      add_func('GetSize_'..tname, HIERARCHY.tables[tname], 'GetSize',
      terra( hdl : ExtStore ) : SizeT
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

      -- refresh indices if appropriate
      escape if Table._is_live then
        local indices   = W._c_cache[Table].dst_indices
        assert(#indices == 1, 'INTERNAL: expect exactly 1 index')
        local Index     = indices[1]
        local iname     = W._c_cache[Index].name
        emit quote  store.[iname]:rebuild(store)  end
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

      -- Per-Row Point Access

      if IS_LIVE then
        -- Per-Row Point Access
        add_func('Read_'..tname..'_is_live', H_FIELDS['is_live'], 'Read',
        terra( hdl : ExtStore, row : KeyT ) : ftype
          var store = to_store(hdl)
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
          return [ W:Read(store, T.row(Table), row, newlist{fname} ) ]
        end)
        add_func('Write_'..tname..'_'..fname, H_FIELDS[fname], 'Write',
        terra( hdl : ExtStore, row : KeyT, val : ftype )
          var store = to_store(hdl)
          return [ W:Write(store, T.row(Table), row, newlist{fname}, val ) ]
        end)

        -- Simple bulk access
        add_func('ReadWriteLock_'..tname..'_'..fname,
                 H_FIELDS[fname], 'ReadWriteLock',
        terra( hdl : ExtStore ) : &ftype
          return to_store(hdl).[tbl_cname].[f_cname]
        end)
        add_func('ReadWriteUnLock_'..tname..'_'..fname,
                 H_FIELDS[fname], 'ReadWriteUnlock',
        terra( hdl : ExtStore )
          -- no-op
        end)
      end

      -- Simple bulk access
      add_func('ReadLock_'..tname..'_'..fname,
               H_FIELDS[fname], 'ReadLock',
      terra( hdl : ExtStore ) : &ftype
        return to_store(hdl).[tbl_cname].[f_cname]
      end)
      add_func('ReadUnLock_'..tname..'_'..fname,
               H_FIELDS[fname], 'ReadUnlock',
      terra( hdl : ExtStore )
        -- no-op
      end)

    end
    add_func_note("")
  end

return FUNCS, STRUCTS, HIERARCHY
end





