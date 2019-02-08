--import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.gpu_datastore"] = Exports

local PARAMETER     = (require 'gong.src.params').get_param
local verbosity     = require('gong.src.verbosity').get_verbosity()

if not PARAMETER('GPU_ENABLED') then return end

local RESIZE_FRACTION   = PARAMETER('RESIZE_FRACTION')
local DEBUG_GPU_MEM     = PARAMETER('DEBUG_GPU_MEM')

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

local CodeGen       = require 'gong.src.codegen'

-----------------------------------------

local C             = require 'gong.src.c'
local assert        = C.assert

local StdContainers = require 'gong.src.stdcontainers'
local vector        = StdContainers.vector

local GPU           = require 'gong.src.gpu_util'

local newlist       = terralib.newlist

local gpu_mem_log       = macro(function(op,tag)
  if DEBUG_GPU_MEM then
    return quote
      C.printf("GPU-MEM-OP   %22s | %s\n", op, tag)
    end
  else return quote end end
end)

-------------------------------------------------------------------------------

local CPU_MASK = terralib.constant(uint8, 0x1)
local GPU_MASK = terralib.constant(uint8, 0x2)
local struct ValidLocation {
  _bits : uint8
}

local Invalid_All     = (`[ValidLocation]{ _bits = 0x0 })
local Valid_All       = (`[ValidLocation]{ _bits = 0x3 })
local Valid_CPU_Only  = (`[ValidLocation]{ _bits = CPU_MASK })
local Valid_GPU_Only  = (`[ValidLocation]{ _bits = GPU_MASK })

terra ValidLocation:ok_GPU()    return (self._bits and GPU_MASK) ~= 0   end
terra ValidLocation:ok_CPU()    return (self._bits and CPU_MASK) ~= 0   end

terra ValidLocation:string()
  if      self._bits == 0x0 then  return "NONE"
  elseif  self._bits == 0x1 then  return "CPU "
  elseif  self._bits == 0x2 then  return "GPU "
  elseif  self._bits == 0x3 then  return "BOTH"
  end
  return "ERR "
end

--[[
Exports.Invalid_All       = Invalid_All
Exports.Valid_All         = Valid_All
Exports.Valid_CPU_Only    = Valid_CPU_Only
Exports.Valid_GPU_Only    = Valid_GPU_Only
--]]


-------------------------------------------------------------------------------

--[[

  GPU Storage Policy.

    How does storage on the GPU differ from the CPU?
  
  Here is a grammatical summary of the abstract data model
    <Schema>  ::= (<Table> `;`)* (<Global> *)
    <Table>   ::= <name> `{` <Field>* `}`
    <Field>   ::= <name> `:` <Type>
    !!! <Index> ::= !!! We currently DO NOT SUPPORT indices on the GPU !!!
    <Global>  ::= <name> `:` <Type>
    <Type>    ::= <primitive>
                | `row(` <Table> `)`
                | `tensor(` <Type> `,` <num>+ `)`
                | `record{` (<name> `:` <Type>)* `}`

  The main issue with our data-structures will be keeping track of mirror
  structures on the CPU and GPU.  To do this, we need a CPU metadata block
  that we can update and vice-versa.

    GMetaData {
      cpu_meta        : GStore
      cpu_globals     : GGlobalFile
      gpu_meta        : &GStore
      gpu_globals     : &GGlobalFile

      meta_valid      : ValidLocation
      globals_valid   : ValidLocation
      store_valid     : GValidStore
    }

  Observe that we have two mirrored blocks of metadata:
      the globals & the storage metadata

  Additionally, we have some bits to track which copies of the metadata
  blocks are valid.  Whenever data is written to on the GPU or CPU
  the bits of the other processor for that data will be invalidated, but
  the data will not be eagerly refreshed.  Then, before launching a
  computation on either processor, we will check the required valid bits
  and lazily refresh data as needed, revalidating the bits.

    GStore {
      <table_name_0>  : GTable_0
      <table_name_1>  : GTable_1
      ...
      <index_name_0>  : IndexType
      <index_name_1>  : IndexType
      ...
    }

    GGlobalFile {
      <global_name_0> : <type_0>
      <global_name_1> : <type_1>
      ...
    }

    GValidStore {
      <table_name_0>  : GValidTable_0
      <table_name_1>  : GValidTable_1
      ...
      <index_name_0>  : ValidLocation
      <index_name_1>  : ValidLocation
      ...
    }

  These decompose over the schema as

    GTable_i {
      alloc_size      : Size_Type_i     -- allocated space
      size            : Size_Type_i     -- used space
      <col_name_0>    : &<type_0>
      <col_name_1>    : &<type_1>
      ...
    }
    If this table is setup for merging then add...
    {
      ...
      old_size        : Size_Type_i     -- temporary storage of prev size
      visited         : bitvector       -- temporary for defining remove rows
      ...
    }

    GValidTable_i {
      size_valid      : ValidLocation   -- if invalid, reallocation needed
      <col_name_0>    : ValidLocation
      <col_name_1>    : ValidLocation
      ...
    }

  These structures must support certain internal operations in addition to
  specifying a data layout.

      Scan( Table, row, body )
      Read( Table, row, path )
     Write( Table, row, path, rval )
    Reduce( Table, op, row, path, rval )
    Insert( Table, vals )

  We don't need to support external API functions on GPU layout
    
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

local gmalloc_ptr = macro(function(typ,N)
  typ = assert(typ:astype())
  return `[&typ](GPU.malloc( sizeof(typ) * [N] ))
end)
local gfree_ptr = macro(function(p)
  return `GPU.free([p])
end)

local WRAPPER_ROOT  = {}
local ERR_BUF_LEN   = 2048


-------------------------------------------------------------------------------
--[[                         Sorted Segment Index                          ]]--
-------------------------------------------------------------------------------

local function SortedSegmentIndex(Index, IndexName)
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

  local CIdx            = terralib.types.newstruct(IndexName..
                                    '_GPU_Sorted_Segment_Index')
  CIdx.entries:insertall {
    -- implicit size and alloced
    { '_segs', &KDST }
  }

  local function PreInstallMethods(GW)
    -- pack into the wrapper...
    GW._c_cache[T0].parallel_indices:insert(Index)
    GW._c_cache[DST].dst_indices:insert(Index)

    local GStore        = GW._c_cache[WRAPPER_ROOT].ctype
    local T0name        = GW._c_cache[T0].name

    terra CIdx:init( store : &GStore )
      assert(store.cpu_meta.[T0name]._size == 0)
      var n_alloc       = store.cpu_meta.[T0name]._alloc_size
      self._segs        = gmalloc_ptr( KDST, n_alloc )
    end
    terra CIdx:destroy( store : &GStore )
      gfree_ptr(self._segs)
      self._segs        = nil
    end

    CIdx.methods.PreInstallMethods = nil -- erase
  end
  CIdx.methods.PreInstallMethods = PreInstallMethods

  local function InstallExtensions(GW, MainW, MAIN_WRAP_ROOT)
    local iname         = GW._c_cache[Index].name

    local CStore        = MainW._c_cache[MAIN_WRAP_ROOT].ctype
    local GStore        = GW._c_cache[WRAPPER_ROOT].GStore
    local T0name        = GW._c_cache[T0].name
    local Dname         = GW._c_cache[DST].name
    local F0name        = GW._c_cache[F0].name
    local F1name        = GW._c_cache[F1].name

    terra CIdx:resize( store    : &CStore )
      -- add 1 so we can generate all segment markers
      var newalloc      = store._gpu_data.cpu_meta.[T0name]._alloc_size + 1
      gfree_ptr(self._segs)
      self._segs        = gmalloc_ptr(KDST, newalloc)
      gpu_mem_log('free-alloc',['resize-index-'..IndexName])
    end

    -- PRECONDITION: data must be sorted on k0 and k1
    terra CIdx:rebuild_already_sorted( store : &CStore )
      var Nseg          = store._gpu_data.cpu_meta.[T0name]._size + 1
      var N             = store._gpu_data.cpu_meta.[Dname]._size
      var key0          = store._gpu_data.cpu_meta.[Dname].[F0name]
      GPU.gen_segments( Nseg, Nseg, N, key0, self._segs )
    end

    -- binary search
    local terra find( self : &CIdx, f1ptr : &K1, k0 : K0, k1 : K1 )
      var lo      = [int32](self._segs[k0])
      var hi      = [int32](self._segs[k0 + 1])-1
      while lo <= hi do
        var mid   = lo + (hi-lo)/2
        var mval  = f1ptr[mid]
        if mval < k1 then
          lo      = mid+1
        elseif mval > k1 then
          hi      = mid-1
        else -- mid == k1
          return true, [uint32](mid)
        end
      end
      return false, [uint32](lo)
    end
    terra CIdx:lookup( f1ptr : &K1, k0 : K0, k1 : K1 ) : {bool,uint32}
      return find(self, f1ptr, k0, k1)
    end

    CIdx.methods.InstallExtensions = nil -- erase
  end
  CIdx.methods.InstallExtensions = InstallExtensions

  return CIdx
end

-------------------------------------------------------------------------------
--[[                           Wrapper Structure                           ]]--
-------------------------------------------------------------------------------

local GWrapper      = {}
GWrapper.__index    = GWrapper

local function GenerateGWrapperStructs(W)
  -- generate structures
  local GStore              = terralib.types.newstruct('GStore')
  local GValidStore         = terralib.types.newstruct('GValidStore')

  for iTable,Table in ipairs(W._tables) do
    local TableName         = Table:name()..'_G'..(iTable-1)
    local TableValidName    = 'valid_'..Table:name()..'_G'..(iTable-1)
    local GTable            = terralib.types.newstruct(TableName)
    local GTableValid       = terralib.types.newstruct(TableValidName)
    local KeyT              = Table:_INTERNAL_terra_key_type()
    local SizeT             = Table:_INTERNAL_terra_size_type()

    GTable.entries:insertall {
      { '_alloc_size',  SizeT },
      { '_size',        SizeT },
    }
    if Table._is_live then
      GTable.entries:insertall  {
        { '_old_size', SizeT   },
        { '_visited',  &uint32 },
      }
    end

    GTableValid.entries:insertall {
      { '_size_valid', ValidLocation }
    }

    local fields            = Table:_INTERNAL_GPU_fields()
    for iField,Field in ipairs(fields) do
      local FieldName       = Field:name()..'_G'..(iField-1)
      local FieldValidName  = 'valid_'..Field:name()..'_G'..(iField-1)
      local FPtr            = &(Field:type():terratype())
      GTable.entries:insert       { '_'..FieldName, FPtr }
      GTableValid.entries:insert  { '_'..FieldValidName, ValidLocation }

      W._c_cache[Field]     = { ctype = FPtr, name = '_'..FieldName      }
      W._cv_cache[Field]    = { ctype = nil,  name = '_'..FieldValidName }
    end

    GTable:complete()
    GTableValid:complete()
    GStore.entries:insert       { '_'..TableName,       GTable      }
    GValidStore.entries:insert  { '_'..TableValidName,  GTableValid }
    W._c_cache[Table]   = { ctype             = GTable,
                            name              = '_'..TableName,
                            parallel_indices  = newlist(),
                            dst_indices       = newlist(), }
    W._cv_cache[Table]  = { ctype = GTableValid, name = '_'..TableValidName }
  end
  for iIndex,Index in ipairs(W._indices) do
    local IndexName         = Index:name()..'_G_index'..(iIndex-1)
    local IndexValidName    = 'valid_'..IndexName
    local GIndex            = SortedSegmentIndex(Index, IndexName)

    GStore.entries:insert       { '_'..IndexName, GIndex }
    GValidStore.entries:insert  { '_'..IndexValidName, ValidLocation }
    W._c_cache[Index]         = { ctype = GIndex, name = '_'..IndexName }
    W._cv_cache[Index]        = { ctype = nil,    name = '_'..IndexValidName }
  end
  GStore:complete()
  GValidStore:complete()

  -- Add Globals in
  local GGlobalFile         = terralib.types.newstruct('GGlobalFile')
  for iGlobal,Global in ipairs(W._globals) do
    local GlobalName        = Global:name()..'_G_global'..(iGlobal-1)
    local GGlobal           = Global:type():terratype()

    GGlobalFile.entries:insert { '_'..GlobalName, GGlobal }
    W._c_cache[Global]      = { ctype = GGlobal, name = '_'..GlobalName }
  end
  GGlobalFile:complete()

  local struct GMetaData {
    cpu_meta        : GStore
    cpu_globals     : GGlobalFile
    gpu_meta        : &GStore
    gpu_globals     : &GGlobalFile

    meta_valid      : ValidLocation
    globals_valid   : ValidLocation
    store_valid     : GValidStore
  }

  W._c_cache[WRAPPER_ROOT]  = { ctype = GMetaData, name = nil,
                                GStore      = GStore,
                                GValidStore = GValidStore,
                                GGlobalFile = GGlobalFile, }
end

local function PreInstallStructMethods(W)
  local GMetaData           = W._c_cache[WRAPPER_ROOT].ctype
  local GStore              = W._c_cache[WRAPPER_ROOT].GStore
  local GValidStore         = W._c_cache[WRAPPER_ROOT].GValidStore
  local GGlobalFile         = W._c_cache[WRAPPER_ROOT].GGlobalFile

  for iIndex,Index in ipairs(W._indices) do
    local GIndex            = W._c_cache[Index].ctype
    GIndex.methods.PreInstallMethods(W)
  end

  -- Initialization and Destruction
  local MIN_INIT_SIZE = 8
  terra GMetaData:init()
    var this                = self

    -- to start, fill out the cpu version of the mirror structures
    escape for iTable,Table in ipairs(W._tables) do
      local tblname           = W._c_cache[Table].name
      local tblvalidname      = W._cv_cache[Table].name

      -- we start with CPU only valid, and empty
      emit quote
        this.cpu_meta.[tblname]._alloc_size         = MIN_INIT_SIZE
        this.cpu_meta.[tblname]._size               = 0
        this.store_valid.[tblvalidname]._size_valid = Valid_CPU_Only
          gpu_mem_log('set-valid-cpu',['init-size-'..Table:name()])
      end

      if Table._is_live then emit quote
        this.cpu_meta.[tblname]._visited
          = gmalloc_ptr(uint32, MIN_INIT_SIZE)
      end end

      local fields            = Table:_INTERNAL_GPU_fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        local fvname          = W._cv_cache[Field].name
        local ftype           = Field:type():terratype()
        emit quote
          this.cpu_meta.[tblname].[fname] = gmalloc_ptr(ftype, MIN_INIT_SIZE)
          this.store_valid.[tblvalidname].[fvname] = Valid_CPU_Only
          gpu_mem_log('set-valid-cpu',['init-'..Field:fullname()])
        end
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
          this.cpu_globals.[globname] = [initval]
        end
      end
    end for iIndex,Index in ipairs(W._indices) do
      local iname             = W._c_cache[Index].name
      local ivname            = W._cv_cache[Index].name
      emit quote
        this.cpu_meta.[iname]:init(this)
        this.store_valid.[ivname] = Valid_CPU_Only
        gpu_mem_log('set-valid-cpu',['init-index-'..Index:fullname()])
      end
    end end

    -- Then, copy over the mirror structures to the GPU
    this.gpu_meta       = gmalloc_ptr(GStore, 1)
    this.gpu_globals    = gmalloc_ptr(GGlobalFile, 1)
    GPU.memcpy_to_gpu(this.gpu_meta, &(this.cpu_meta),
                      sizeof(GStore))
    GPU.memcpy_to_gpu(this.gpu_globals, &(this.cpu_globals),
                      sizeof(GGlobalFile))
    gpu_mem_log('all-data','INTIIALIZE')
    self.meta_valid     = Valid_All
    self.globals_valid  = Valid_All
    gpu_mem_log('set-valid-all','init-meta-data')
    gpu_mem_log('set-valid-all','init-globals')
  end
  terra GMetaData:destroy()
    var this                  = self
    escape for iTable,Table in ipairs(W._tables) do
      local tblname           = W._c_cache[Table].name
      if Table._is_live then emit quote
        gfree_ptr( this.cpu_meta.[tblname]._visited )
      end end
      local fields            = Table:_INTERNAL_GPU_fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        emit quote
          gfree_ptr( this.cpu_meta.[tblname].[fname] )
          this.cpu_meta.[tblname].[fname] = nil -- unnecessary defensiveness
        end
      end
    end for iIndex,Index in ipairs(W._indices) do
      local iname             = W._c_cache[Index].name
      emit quote
        this.cpu_meta.[iname]:destroy(this)
      end
    end end

    gfree_ptr(this.gpu_globals)
    gfree_ptr(this.gpu_meta)
    gpu_mem_log('all-data','DESTROY')
  end
end

local function Install_Data_Extensions(W, MainW, MAIN_WRAP_ROOT)
  W._main_wrap              = MainW
  local CStore              = MainW._c_cache[MAIN_WRAP_ROOT].ctype

  local GMetaData           = W._c_cache[WRAPPER_ROOT].ctype
  local GStore              = W._c_cache[WRAPPER_ROOT].GStore
  local GValidStore         = W._c_cache[WRAPPER_ROOT].GValidStore
  local GGlobalFile         = W._c_cache[WRAPPER_ROOT].GGlobalFile

  for iIndex,Index in ipairs(W._indices) do
    local GIndex            = W._c_cache[Index].ctype
    GIndex.methods.InstallExtensions(W, MainW, MAIN_WRAP_ROOT)
  end

  -- debug
  terra GMetaData:debug_dump_valids()
    var this = self
    C.printf("dumping valid data...\n")
    C.printf("  %s : meta_valid\n", self.meta_valid:string())
    C.printf("  %s : globals_valid\n", self.globals_valid:string())

    escape for iTable,Table in ipairs(W._tables) do
      local tblvalidname      = W._cv_cache[Table].name
      emit quote
        var v = this.store_valid.[tblvalidname]._size_valid
        C.printf("  %s\n", tblvalidname)
        C.printf("    %s : size_valid\n", v:string())
      end

      local fields            = Table:_INTERNAL_GPU_fields()
      for iField,Field in ipairs(fields) do
        local fvname          = W._cv_cache[Field].name
        emit quote
          var fv = this.store_valid.[tblvalidname].[fvname]
          C.printf("    %s : %s\n", fv:string(), fvname)
        end
      end

    end for iIndex,Index in ipairs(W._indices) do
      local ivname            = W._cv_cache[Index].name
      emit quote
        var iv = this.store_valid.[ivname]
        C.printf("  %s : %s\n", iv:string(), ivname)
      end
    end end
  end
  terra GMetaData:debug_dump_cpu_meta()
    var this = self
    C.printf("dumping cpu-side meta-data...\n")

    escape for iTable,Table in ipairs(W._tables) do
      local tblname           = W._c_cache[Table].name
      emit quote
        var a = this.cpu_meta.[tblname]._alloc_size
        var s = this.cpu_meta.[tblname]._size
        C.printf("  %s\n", tblname)
        C.printf("    %9d : alloc_size\n", a)
        C.printf("    %9d : size\n", s)
      end
      if Table._is_live then emit quote
        C.printf("    %9d : old_size\n", this.cpu_meta.[tblname]._old_size)
        C.printf("    %p  : visited\n",  this.cpu_meta.[tblname]._visited)
      end end

      local fields            = Table:_INTERNAL_GPU_fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        emit quote
          var fp = this.cpu_meta.[tblname].[fname]
          C.printf("    %p : %s\n", fp, fname)
        end
      end
    
    end for iIndex,Index in ipairs(W._indices) do
      local iname             = W._c_cache[Index].name
      emit quote
        var ip = this.cpu_meta.[iname]._segs
        C.printf("  %p : %s\n", ip, iname)
      end
    end end
  end

  -- data movement helper functions
  terra CStore:meta_to_gpu()
    -- short-circuit
    if self._gpu_data.meta_valid:ok_GPU() then return end
    -- sanity
    assert(self._gpu_data.meta_valid:ok_CPU(),
           'INTERNAL: cannot copy to gpu -- CPU metadata invalid')
    GPU.memcpy_to_gpu(self._gpu_data.gpu_meta,
                      &(self._gpu_data.cpu_meta),
                      sizeof(GStore))
    gpu_mem_log('memcpy_to_gpu','meta-data')
    self._gpu_data.meta_valid = Valid_All
    gpu_mem_log('set-valid-all','meta-data')
  end
  terra CStore:meta_from_gpu()
    -- short-circuit
    if self._gpu_data.meta_valid:ok_CPU() then return end
    -- sanity
    assert(self._gpu_data.meta_valid:ok_GPU(),
           'INTERNAL: cannot copy from gpu -- GPU metadata invalid')
    GPU.memcpy_from_gpu(&(self._gpu_data.cpu_meta),
                        self._gpu_data.gpu_meta,
                        sizeof(GStore))
    gpu_mem_log('memcpy_from_gpu','meta-data')
    self._gpu_data.meta_valid = Valid_All
    gpu_mem_log('set-valid-all','meta-data')
  end
  terra CStore:globals_to_gpu()
    -- short-circuit
    if self._gpu_data.globals_valid:ok_GPU() then return end
    -- sanity
    assert(self._gpu_data.globals_valid:ok_CPU(),
           'INTERNAL: cannot copy to gpu -- CPU globals invalid')
    -- Gather global data from the CPU structure locations
    escape for iGlobal,Global in ipairs(W._globals) do
      local main_globname     = MainW._c_cache[Global].name
      local sub_globname      = W._c_cache[Global].name
      emit quote
        self._gpu_data.cpu_globals.[sub_globname] = self.[main_globname]
      end
    end end

    GPU.memcpy_to_gpu(self._gpu_data.gpu_globals,
                      &(self._gpu_data.cpu_globals),
                      sizeof(GGlobalFile))
    gpu_mem_log('memcpy_to_gpu','globals')
    self._gpu_data.globals_valid = Valid_All
    gpu_mem_log('set-valid-all','globals')
  end
  terra CStore:globals_from_gpu()
    -- short-circuit
    if self._gpu_data.globals_valid:ok_CPU() then return end
    -- sanity
    assert(self._gpu_data.globals_valid:ok_GPU(),
           'INTERNAL: cannot copy from gpu -- GPU globals invalid')
    GPU.memcpy_from_gpu(&(self._gpu_data.cpu_globals),
                        self._gpu_data.gpu_globals,
                        sizeof(GGlobalFile))
    gpu_mem_log('memcpy_from_gpu','globals')
    self._gpu_data.globals_valid = Valid_All
    gpu_mem_log('set-valid-all','globals')

    -- Scatter global data back into the CPU structure locations
    escape for iGlobal,Global in ipairs(W._globals) do
      local main_globname     = MainW._c_cache[Global].name
      local sub_globname      = W._c_cache[Global].name
      emit quote
        self.[main_globname]  = self._gpu_data.cpu_globals.[sub_globname]
      end
    end end
  end

  -- Per-Table Methods
  for iTable,Table in ipairs(W._tables) do
    local GTable            = W._c_cache[Table].ctype
    local SizeT             = Table:_INTERNAL_terra_size_type()
    local MAXsize           = Table:_INTERNAL_terra_MAX_SIZE()
    local main_tblname      = MainW._c_cache[Table].name
    local sub_tblname       = W._c_cache[Table].name
    local tblvalidname      = W._cv_cache[Table].name

    local fields            = Table:_INTERNAL_GPU_fields()
    -- indices that are keyed off this table (parallel) or
    -- indices that lookup rows of this table (dst)
    local indices           = W._c_cache[Table].parallel_indices
    local dst_indices       = W._c_cache[Table].dst_indices
    assert(#dst_indices <= 1, 'INTERNAL: expect max 1 index on table')
    local DST_Index         = dst_indices[1]

    GTable.methods.size     = macro(function(self) return `self._size end)

    -- set size to provided parameter and possibly re-allocate
    --    - ALLOC must be at least the provided size
    --    - ALLOC must be at least the derived size function when
    --            it's been provided
    -- make sure that the backing memory for fields is of adequate size
    terra GTable:resize(storeptr : &CStore, size : SizeT)
      -- remember these in case it's useful
      var oldalloc  = storeptr._gpu_data.cpu_meta.[sub_tblname]._alloc_size
      var oldsize   = storeptr._gpu_data.cpu_meta.[sub_tblname]._size

      -- determine the new allocation and size
      var newsize   = size --storeptr.[main_tblname]._size
      var newalloc  = [ W:_INTERNAL_Size_Func(storeptr, T.row(Table)) or
                        (`[SizeT](0)) ]
      if newalloc < newsize then newalloc = newsize end
      -- only re-allocate on sufficient shrinking
      if newalloc < oldalloc and newalloc > oldalloc*RESIZE_FRACTION then
        newalloc    = oldalloc
      end
      -- commit new sizes to the metadata block
      storeptr._gpu_data.cpu_meta.[sub_tblname]._alloc_size   = newalloc
      storeptr._gpu_data.cpu_meta.[sub_tblname]._size         = newsize

      -- if a re-allocation has actually been requested, then...
      if newalloc ~= oldalloc then escape
        for iField,Field in ipairs(fields) do
          local fname         = W._c_cache[Field].name
          local fvname        = W._cv_cache[Field].name
          local ftype         = Field:type():terratype()
          emit quote
            var newdata       : &ftype
            var olddata       = storeptr._gpu_data.cpu_meta
                                        .[sub_tblname].[fname]
            -- decide whether we need to preserve data through reallocation
            var copysize      = newsize
            if not storeptr._gpu_data.store_valid
                           .[tblvalidname].[fvname]:ok_GPU()
            then copysize = 0 end
            if oldsize < copysize then copysize = oldsize end

            -- copy or not
            if copysize > 0 then
              newdata         = gmalloc_ptr(ftype, newalloc)
              GPU.memcpy_on_gpu(newdata, olddata, sizeof(ftype)*copysize)
              gfree_ptr(olddata)
              gpu_mem_log('alloc-copy-free',['resize-'..Field:fullname()])
            else
              gfree_ptr(olddata)
              newdata = gmalloc_ptr(ftype, newalloc)
              gpu_mem_log('free-alloc',['resize-'..Field:fullname()])
            end
            -- commit new data pointer
            storeptr._gpu_data.cpu_meta.[sub_tblname].[fname] = newdata
          end
        end
        -- make sure to reallocate the visited array
        if Table._is_live then
          emit quote
            gfree_ptr( storeptr._gpu_data.cpu_meta.[sub_tblname]._visited )
            var visited_alloc   = newalloc/32 + 1 -- safety for rounding
            storeptr._gpu_data.cpu_meta.[sub_tblname]
                              ._visited = gmalloc_ptr(uint32, visited_alloc)
            gpu_mem_log('free-alloc',['resize-is-livetable-'..Table:name()])
          end
        end
        -- also need to determine whether there are indices that are
        -- sized based on this table
        for iIndex,Index in ipairs(indices) do
          local iname         = W._c_cache[Index].name
          local ivname        = W._cv_cache[Index].name
          emit quote
            storeptr._gpu_data.cpu_meta.[iname]:resize(storeptr)
            var valid_loc     = storeptr._gpu_data.store_valid.[ivname]
            if valid_loc:ok_CPU() then
              storeptr._gpu_data.store_valid.[ivname] = Valid_CPU_Only
              gpu_mem_log('set-valid-cpu',['resize-index-'..Index:fullname()])
            else
              storeptr._gpu_data.store_valid.[ivname] = Invalid_All
              gpu_mem_log('set-valid-none',['resize-index-'..Index:fullname()])
            end
          end
        end
      end end
    end

    -- movement helpers
    terra GTable:size_to_gpu(storeptr : &CStore)
      -- short-circuit
      if storeptr._gpu_data.store_valid.[tblvalidname]
                                       ._size_valid:ok_GPU() then return end
      -- sanity
      assert(storeptr._gpu_data.store_valid.[tblvalidname]
                                           ._size_valid:ok_CPU(),
             'INTERNAL: cannot copy to GPU -- CPU tablesize invalid')
      -- sync any GPU metadata we're missing...
      storeptr:meta_from_gpu()

      var newsize : SizeT
      escape if Table._is_live then
        local ivname        = W._cv_cache[DST_Index].name
      emit quote
        -- if we changed the size on the CPU, we should probably
        -- have a valid index...
        assert(storeptr._gpu_data.store_valid.[ivname]:ok_CPU() and
               not storeptr._gpu_data.store_valid.[ivname]:ok_GPU(),
               'INTERNAL: expect index valid on CPU only')
        -- ensure data is compacted before copying size over
        [ W._main_wrap:_INTERNAL_EnsureCompact( storeptr, T.row(Table) ) ]
        newsize = storeptr.[main_tblname]:n_rows()
      end else emit quote
        newsize = storeptr.[main_tblname]:size()
      end end end
      self:resize(storeptr, newsize)

      -- sanity check the field data
      escape for iField,Field in ipairs(fields) do
        local fvname        = W._cv_cache[Field].name
        emit quote
          -- sanity check
          assert(storeptr._gpu_data.store_valid.[tblvalidname]
                                               .[fvname]:ok_CPU() and
             not storeptr._gpu_data.store_valid.[tblvalidname]
                                               .[fvname]:ok_GPU(),
                 'INTERNAL: expected field to be CPU valid only')
        end
      end end

      -- finally, invalidate the metadata block on the GPU
      -- and validate the table size
      storeptr._gpu_data.store_valid.[tblvalidname]
                                    ._size_valid      = Valid_All
      storeptr._gpu_data.meta_valid                   = Valid_CPU_Only
      gpu_mem_log('set-valid-all',['size-'..Table:name()])
      gpu_mem_log('set-valid-cpu',['meta-data'])
    end

    terra GTable:size_from_gpu(storeptr : &CStore)
      --storeptr._gpu_data:debug_dump_valids()
      --storeptr._gpu_data:debug_dump_cpu_meta()
      -- short-circuit
      if storeptr._gpu_data.store_valid.[tblvalidname]
                                       ._size_valid:ok_CPU() then return end
      -- sanity
      assert(storeptr._gpu_data.store_valid.[tblvalidname]
                                           ._size_valid:ok_GPU(),
             'INTERNAL: cannot copy to CPU -- GPU tablesize invalid')
      -- sync the GPU metadata with the correct size
      storeptr:meta_from_gpu()

      -- we can now reallocate the CPU data
      var newsize   = storeptr._gpu_data.cpu_meta.[sub_tblname]._size      
      escape if Table._is_live then
        local ivname        = W._cv_cache[DST_Index].name
      emit quote
        assert(storeptr._gpu_data.store_valid.[ivname]:ok_GPU() and
               not storeptr._gpu_data.store_valid.[ivname]:ok_CPU(),
               'INTERNAL: expect index to be valid on the GPU only')

        [ MainW:_INTERNAL_ReserveRows(storeptr, T.row(Table), newsize) ]
        -- the data will be sorted by virtue of the indexing scheme here
        storeptr.[main_tblname]._is_sorted = true
      end else emit quote
        storeptr.[main_tblname]:resize(storeptr, newsize)
      end end end

      -- sanity check all the field data...
      escape for iField,Field in ipairs(fields) do
        local fvname          = W._cv_cache[Field].name
        emit quote
          -- sanity check
          assert(storeptr._gpu_data.store_valid.[tblvalidname]
                                               .[fvname]:ok_GPU() and
             not storeptr._gpu_data.store_valid.[tblvalidname]
                                               .[fvname]:ok_CPU(),
                 'INTERNAL: expected field to GPU valid only')
        end
      end end

      -- finally, validate the table size
      storeptr._gpu_data.store_valid.[tblvalidname]._size_valid = Valid_All
      gpu_mem_log('set-valid-all',['size-'..Table:name()])
    end

    if Table._is_live then
      local iname           = W._c_cache[DST_Index].name
      local ivname          = W._cv_cache[DST_Index].name
      local cpu_iname       = W._main_wrap._c_cache[DST_Index].name

      local f0,f1           = unpack(Table:primary_key())
      local f0_name         = W._c_cache[f0].name
      local f1_name         = W._c_cache[f1].name
      local f0_valid_name   = W._cv_cache[f0].name
      local f1_valid_name   = W._cv_cache[f1].name
      local f0_main_name    = W._main_wrap._c_cache[f0].name
      local f1_main_name    = W._main_wrap._c_cache[f1].name
      local f0typ           = f0:type():terratype()
      local f1typ           = f1:type():terratype()

      terra GTable:index_to_gpu(storeptr : &CStore)
        -- short-circuit
        if storeptr._gpu_data.store_valid.[ivname]:ok_GPU() then return end
        -- sanity
        assert(storeptr._gpu_data.store_valid.[ivname]:ok_CPU(),
               'INTERNAL: cannot copy to GPU -- CPU index invalid')
        assert(storeptr._gpu_data.store_valid.[tblvalidname]
                                             ._size_valid:ok_GPU(),
               'INTERNAL: GPU size must already be valid')
        -- sanity check the field data
        escape for iField,Field in ipairs(fields) do
          local fvname        = W._cv_cache[Field].name
          emit quote
            -- sanity check
            assert(storeptr._gpu_data.store_valid.[tblvalidname]
                                                 .[fvname]:ok_CPU() and
               not storeptr._gpu_data.store_valid.[tblvalidname]
                                                 .[fvname]:ok_GPU(),
                   'INTERNAL: expected field to be CPU valid only')
          end
        end end

        -- Ok, so the size has been moved over, but not the index or
        -- any fields.  In this case, we can ensure the CPU data is sorted,
        -- copy the sorted indexed fields, and then reconstruct a GPU index.
        [ W._main_wrap:_INTERNAL_EnsureSorted( storeptr, T.row(Table) ) ]

        -- move the two key fields over to the GPU in order to read them
        var tblsize = storeptr._gpu_data.cpu_meta.[sub_tblname]._size
        var f0_cpu  = storeptr.[main_tblname].[f0_main_name]
        var f0_gpu  = storeptr._gpu_data.cpu_meta.[sub_tblname].[f0_name]
        var f1_cpu  = storeptr.[main_tblname].[f1_main_name]
        var f1_gpu  = storeptr._gpu_data.cpu_meta.[sub_tblname].[f1_name]

        GPU.memcpy_to_gpu(f0_gpu, f0_cpu, sizeof(f0typ)*tblsize)
        GPU.memcpy_to_gpu(f1_gpu, f1_cpu, sizeof(f1typ)*tblsize)
        gpu_mem_log('memcpy_to_gpu',['index_to_gpu-'..iname])
        storeptr._gpu_data.store_valid.[tblvalidname]
                                      .[f0_valid_name]  = Valid_All
        storeptr._gpu_data.store_valid.[tblvalidname]
                                      .[f1_valid_name]  = Valid_All
        gpu_mem_log('set-valid-all',[f0:fullname()])
        gpu_mem_log('set-valid-all',[f1:fullname()])

        storeptr._gpu_data.cpu_meta.[iname]:rebuild_already_sorted(storeptr)

        -- validate the index on the GPU
        storeptr._gpu_data.store_valid.[ivname]         = Valid_All
        gpu_mem_log('set-valid-all',['index-'..DST_Index:fullname()])
      end

      terra GTable:index_from_gpu(storeptr : &CStore)
        -- short-circuit
        if storeptr._gpu_data.store_valid.[ivname]:ok_CPU() then return end
        -- sanity
        assert(storeptr._gpu_data.store_valid.[ivname]:ok_GPU(),
               'INTERNAL: cannot copy to CPU -- GPU index invalid')
        assert(storeptr._gpu_data.store_valid.[tblvalidname]
                                             ._size_valid:ok_CPU(),
               'INTERNAL: CPU size must already be valid')

        -- We know that the size has been moved over, but not the index.
        -- We can just move the two indexed fields over, and then
        -- reconstruct the CPU index
        var tblsize = storeptr._gpu_data.cpu_meta.[sub_tblname]._size
        var f0_cpu  = storeptr.[main_tblname].[f0_main_name]
        var f0_gpu  = storeptr._gpu_data.cpu_meta.[sub_tblname].[f0_name]
        var f1_cpu  = storeptr.[main_tblname].[f1_main_name]
        var f1_gpu  = storeptr._gpu_data.cpu_meta.[sub_tblname].[f1_name]

        GPU.memcpy_from_gpu(f0_cpu, f0_gpu, sizeof(f0typ)*tblsize)
        GPU.memcpy_from_gpu(f1_cpu, f1_gpu, sizeof(f1typ)*tblsize)
        gpu_mem_log('memcpy_from_gpu',['index_from_gpu-'..iname])
        storeptr._gpu_data.store_valid.[tblvalidname]
                                      .[f0_valid_name]  = Valid_All
        storeptr._gpu_data.store_valid.[tblvalidname]
                                      .[f1_valid_name]  = Valid_All
        gpu_mem_log('set-valid-all',[f0:fullname()])
        gpu_mem_log('set-valid-all',[f1:fullname()])

        storeptr.[cpu_iname]:rebuild(storeptr)

        -- validate the index on the CPU
        storeptr._gpu_data.store_valid.[ivname]         = Valid_All
        gpu_mem_log('set-valid-all',['index-'..DST_Index:fullname()])
      end
    end
  end
end

local function NewSubWrapper(args)
  --assert(#args.indices == 0,
  --       'INTERNAL: GPU does not support indices currently')

  local W = setmetatable({
    _tables         = args.tables:copy(),
    _globals        = args.globals:copy(),
    _indices        = args.indices:copy(),
    _c_cache        = {},
    _cv_cache       = {}, -- for the valid stuff
    _func_cache     = {},
    _cuda_loaders   = newlist{ GPU.init_cudakernels },
  }, GWrapper)

  -- cannot refer to data from _main_wrap in these calls
  GenerateGWrapperStructs(W)
  PreInstallStructMethods(W)

  return W
end
Exports.GenerateSubWrapper  = NewSubWrapper

Exports.Install_Extensions  = Install_Data_Extensions

-------------------------------------------------------------------------------
--[[                       Code Generation Interface                       ]]--
-------------------------------------------------------------------------------



local function unpack_prepare_tbl(W, tbltype)
  local Table         = tbltype:table()
  local main_tblname  = W._main_wrap._c_cache[Table].name
  local sub_tblname   = W._c_cache[Table].name
  local tblvalidname  = W._cv_cache[Table].name
  return Table, main_tblname, sub_tblname, tblvalidname
end
local function prepare_prelude(W, storeptr, tbltype, field_name)
  local Table, main_tblname, sub_tblname, tblvalidname
                      = unpack_prepare_tbl(W, tbltype)
  local Field         = assert(Table:fields(field_name),
                               'INTERNAL: field lookup failed')
  local fcname        = W._main_wrap._c_cache[Field].name
  local fgname        = W._c_cache[Field].name
  local fvname        = W._cv_cache[Field].name

  local metatbl       = (`storeptr._gpu_data.cpu_meta.[sub_tblname])
  local fvalid        = (`storeptr._gpu_data.store_valid.[tblvalidname]
                                                        .[fvname])
  local fc_ptr        = (`storeptr.[main_tblname].[fcname])
  local fg_ptr        = (`storeptr._gpu_data.cpu_meta.[sub_tblname]
                                                     .[fgname])

  local ftype         = Field:type():terratype()
  local fbytes        = (`sizeof(ftype) *
                          storeptr._gpu_data.cpu_meta.[sub_tblname]._size)

  return metatbl, fbytes, fvalid, fc_ptr, fg_ptr
end

function GWrapper:_INTERNAL_Size_Func(storeptr, tbltype)
  local W             = self
  local Table         = unpack_prepare_tbl(W, tbltype)
  local SizeT         = Table:_INTERNAL_terra_size_type()
  local sztbl         = Table._size_func
  -- short-circuit when the size function doesn't exist...
  if not sztbl then return nil end

  local szexpr        = (`[SizeT]([sztbl.offset]))
  -- Where is the size of the other table freshest?
  -- oh, it must be fine on the GPU after a meta sync...
  if sztbl.tblB then
    local Bname     = W._c_cache[sztbl.tblB].name
    szexpr = `([sztbl.szB]*storeptr._gpu_data.cpu_meta
                                   .[Bname]._size + [szexpr])
  end
  if sztbl.tblA then
    local Aname     = W._c_cache[sztbl.tblA].name
    szexpr = `([sztbl.szA]*storeptr._gpu_data.cpu_meta
                                   .[Aname]._size + [szexpr])
  end
  return szexpr
end



-------------------------------------------------------------------------------
--[[        Wrapper Helper Functions; INTERNAL to DataStore modules        ]]--
-------------------------------------------------------------------------------

function GWrapper:PrepareReadWriteGlobals(storeptr, on_gpu)
  if on_gpu then
    return quote  storeptr:globals_to_gpu()
                  storeptr._gpu_data.globals_valid = Valid_GPU_Only
                  gpu_mem_log('set-valid-gpu',['readwrite-globals'])
             end
  else
    return quote  storeptr:globals_from_gpu()
                  storeptr._gpu_data.globals_valid = Valid_CPU_Only
                  gpu_mem_log('set-valid-cpu',['readwrite-globals'])
             end
  end
end
function GWrapper:PrepareReadGlobals(storeptr, on_gpu)
  if on_gpu then
    return quote  storeptr:globals_to_gpu()   end
  else
    return quote  storeptr:globals_from_gpu() end
  end
end

local function handle_is_live_edge_case(self, storeptr, tbltype, on_gpu)
  local Table, main_tblname, sub_tblname, tblvalidname
                      = unpack_prepare_tbl(self, tbltype)
  local metatbl       = (`storeptr._gpu_data.cpu_meta.[sub_tblname])
  if on_gpu then  return quote [metatbl]:size_to_gpu(storeptr) end
            else  return quote [metatbl]:size_from_gpu(storeptr) end end
end

function GWrapper:PrepareOverWrite(storeptr, tbltype, field_name, on_gpu)
  local W         = self
  if field_name == 'is_live' then
    return handle_is_live_edge_case(W, storeptr, tbltype, on_gpu) end
  local metatbl, fbytes, fvalid, fc_ptr, fg_ptr
                  = prepare_prelude(W, storeptr, tbltype, field_name)
  local ffullname = tbltype:table()[field_name]:fullname()

  -- no need to copy; only invalidate
  if on_gpu then
    return quote  [metatbl]:size_to_gpu(storeptr)
                  fvalid = Valid_GPU_Only
                  gpu_mem_log('set-valid-gpu',['overwrite-'..ffullname])
             end
  else
    return quote  [metatbl]:size_from_gpu(storeptr)
                  fvalid = Valid_CPU_Only
                  gpu_mem_log('set-valid-cpu',['overwrite-'..ffullname])
             end
  end
end

function GWrapper:PrepareReadWrite(storeptr, tbltype, field_name, on_gpu)
  local W         = self
  if field_name == 'is_live' then
    return handle_is_live_edge_case(W, storeptr, tbltype, on_gpu) end
  local metatbl, fbytes, fvalid, fc_ptr, fg_ptr
                  = prepare_prelude(W, storeptr, tbltype, field_name)
  local ffullname = tbltype:table()[field_name]:fullname()

  -- need to copy & invalidate
  if on_gpu then
    return quote  [metatbl]:size_to_gpu(storeptr)
                  if not fvalid:ok_GPU() then
                    assert(fvalid:ok_CPU(), -- sanity check
                           'INTERNAL: expect field valid on CPU')
                    GPU.memcpy_to_gpu(fg_ptr, fc_ptr, fbytes)
                    gpu_mem_log('memcpy_to_gpu',
                                ['readwrite-'..tbltype:table():name()..
                                 '.'..field_name])
                  end
                  fvalid = Valid_GPU_Only
                  gpu_mem_log('set-valid-gpu',['readwrite-'..ffullname])
             end
  else
    return quote  [metatbl]:size_from_gpu(storeptr)
                  if not fvalid:ok_CPU() then
                    assert(fvalid:ok_GPU(), -- sanity check
                           'INTERNAL: expect field valid on GPU')
                    GPU.memcpy_from_gpu(fc_ptr, fg_ptr, fbytes)
                    gpu_mem_log('memcpy_from_gpu',
                                ['readwrite-'..tbltype:table():name()..
                                 '.'..field_name])
                  end
                  fvalid = Valid_CPU_Only
                  gpu_mem_log('set-valid-cpu',['readwrite-'..ffullname])
             end
  end
end

function GWrapper:PrepareRead(storeptr, tbltype, field_name, on_gpu)
  local W         = self
  if field_name == 'is_live' then
    return handle_is_live_edge_case(W, storeptr, tbltype, on_gpu) end
  local metatbl, fbytes, fvalid, fc_ptr, fg_ptr
                  = prepare_prelude(W, storeptr, tbltype, field_name)
  local ffullname = tbltype:table()[field_name]:fullname()

  -- need to copy; no need to invalidate
  if on_gpu then
    return quote  [metatbl]:size_to_gpu(storeptr)
                  if not fvalid:ok_GPU() then
                    assert(fvalid:ok_CPU(), -- sanity check
                           'INTERNAL: expect field valid on CPU')
                    GPU.memcpy_to_gpu(fg_ptr, fc_ptr, fbytes)
                    gpu_mem_log('memcpy_to_gpu',
                                ['read-'..tbltype:table():name()..
                                 '.'..field_name])
                    fvalid = Valid_All
                    gpu_mem_log('set-valid-all',['read-'..ffullname])
                  end end
  else
    return quote  [metatbl]:size_from_gpu(storeptr)
                  if not fvalid:ok_CPU() then
                    assert(fvalid:ok_GPU(), -- sanity check
                           'INTERNAL: expect field valid on GPU')
                    GPU.memcpy_from_gpu(fc_ptr, fg_ptr, fbytes)
                    gpu_mem_log('memcpy_from_gpu',
                                ['read-'..tbltype:table():name()..
                                 '.'..field_name])
                    fvalid = Valid_All
                    gpu_mem_log('set-valid-all',['read-'..ffullname])
                  end end
  end
end

function GWrapper:PrepareFinal(storeptr, on_gpu, gpu_size_modified)
  if on_gpu then
    local meta_refresh = quote storeptr:meta_to_gpu() end
    -- if the size is being modified, make sure to invalidate the
    -- CPU-side metadata to force a refresh of it when next needed
    local cpu_meta_invalidate = quote
      storeptr._gpu_data.meta_valid = Valid_GPU_Only
      gpu_mem_log('set-valid-gpu',['meta-data (final)'])
    end
    if not gpu_size_modified then cpu_meta_invalidate = quote end end
    return quote
      [meta_refresh]
      [cpu_meta_invalidate]

      --storeptr._gpu_data:debug_dump_valids()
      --storeptr._gpu_data:debug_dump_cpu_meta()
    end
  else
    return quote end
  end
end

function GWrapper:Clear(storeptr, tbltype, on_gpu)
  local W             = self
  local Table, main_tblname, sub_tblname, tblvalidname
                      = unpack_prepare_tbl(W, tbltype)
  local fields        = Table:_INTERNAL_GPU_fields()

  local ivname        = nil
  local Index         = nil
  if Table._is_live then
    local indices     = W._c_cache[Table].dst_indices
    assert(#indices == 1, 'INTERNAL: expect exactly 1 index')
    Index             = indices[1]
    local ivname      = W._cv_cache[Index].name
  end

  if on_gpu then
    return quote
      -- make sure that the metadata block is fresh before we
      -- start reading from or writing to it
      storeptr:meta_from_gpu()

      -- we can request that the size be zero...
      storeptr._gpu_data.cpu_meta.[sub_tblname]:resize(storeptr, 0)

      -- since we wrote to the metadatablock, mark it for refresh onto the GPU
      storeptr._gpu_data.meta_valid = Valid_CPU_Only
      gpu_mem_log('set-valid-cpu',['meta-data (clear)'])

      -- but note that the size on the CPU is now invalid
      storeptr._gpu_data.store_valid.[tblvalidname]
                                    ._size_valid = Valid_GPU_Only
      gpu_mem_log('set-valid-gpu',['size-'..Table:name()..' (clear)'])
      -- then we need to prepare to overwrite the data in question
      escape for iField,Field in ipairs(fields) do
        local fname           = Field:name()
        emit(W:PrepareOverWrite(storeptr, tbltype, fname, on_gpu))
      end end

      -- mark the index as valid on the gpu only
      escape if Table._is_live then emit quote
        storeptr._gpu_data.store_valid.[ivname] = Valid_GPU_Only
        gpu_mem_log('set-valid-gpu',['index-'..Index:fullname()..' (clear)'])
      end end end
    end
  else
    -- on cpu, use the existing routines to do the actual clear
    return quote
      [ W._main_wrap:Clear(storeptr, tbltype) ]
      -- note that the size on the GPU is now invalid
      storeptr._gpu_data.store_valid.[tblvalidname]
                                    ._size_valid = Valid_CPU_Only
      gpu_mem_log('set-valid-cpu',['size-'..Table:name()..' (clear)'])
      -- then we need to prepare to read-write the data in question
      escape for iField,Field in ipairs(fields) do
        local fname           = Field:name()
        emit(W:PrepareOverWrite(storeptr, tbltype, fname, on_gpu))
      end end

      -- mark the index as valid on the cpu only
      escape if Table._is_live then emit quote
        storeptr._gpu_data.store_valid.[ivname] = Valid_CPU_Only
        gpu_mem_log('set-valid-cpu',['index-'..Index:fullname()..' (clear)'])
      end end end
    end
  end
end

function GWrapper:CPU_SizeRefresh(storeptr, tbltype)
  local W             = self
  local Table, main_tblname, sub_tblname, tblvalidname
                      = unpack_prepare_tbl(W, tbltype)
  local metatbl       = (`storeptr._gpu_data.cpu_meta.[sub_tblname])
  return quote
    [metatbl]:size_from_gpu(storeptr)
    --storeptr._gpu_data:debug_dump_valids()
  end
end

function GWrapper:CPU_PrepareLoadTable(storeptr, tbltype)
  local Table, main_tblname, sub_tblname, tblvalidname
                      = unpack_prepare_tbl(self, tbltype)
  local fields        = Table:_INTERNAL_GPU_fields()
  -- invalidate the GPU data
  -- and validate the CPU data fields...
  local stmts = newlist()
  stmts:insert(quote 
    storeptr._gpu_data.store_valid.[tblvalidname]
                                  ._size_valid = Valid_CPU_Only
    gpu_mem_log('set-valid-cpu',['load-size-'..Table:name()])
  end)
  for iField,Field in ipairs(fields) do
    local fvname      = self._cv_cache[Field].name
    stmts:insert(quote
      storeptr._gpu_data.store_valid.[tblvalidname]
                                    .[fvname] = Valid_CPU_Only
      gpu_mem_log('set-valid-cpu',['load-'..Field:fullname()])
    end)
  end
  return quote [stmts] end
end

function GWrapper:CPU_index_rebuild(storeptr, Index)
  local ivname        = self._cv_cache[Index].name
  return quote
    storeptr._gpu_data.store_valid.[ivname]   = Valid_CPU_Only
    gpu_mem_log('set-valid-cpu',['rebuild-index-'..Index:fullname()])
  end
end

function GWrapper:PreMerge(storeptr, tbltype, on_gpu)
  local W             = self
  local Table, main_tblname, sub_tblname, tblvalidname
                      = unpack_prepare_tbl(W, tbltype)
  local fields        = Table:_INTERNAL_GPU_fields()
  local dst_indices   = W._c_cache[Table].dst_indices
  assert(#dst_indices == 1, 'INTERNAL: expected exactly one index')
  local Index         = dst_indices[1]
  local iname         = W._c_cache[Index].name
  local ivname        = W._cv_cache[Index].name
  local main_iname    = W._main_wrap._c_cache[Index].name

  if on_gpu then
    return quote
      -- make sure that the metadata block is fresh before we
      -- start reading from or writing to it
      storeptr:meta_from_gpu()

      -- make sure the table size and index is fresh
      if not storeptr._gpu_data.store_valid.[tblvalidname]
                                           ._size_valid:ok_GPU()
      then
        assert(storeptr._gpu_data.store_valid.[tblvalidname]
                                          ._size_valid:ok_CPU(),
               'INTERNAL: expect size valid on CPU')
        -- get the size and index over to the GPU
        storeptr._gpu_data.cpu_meta.[sub_tblname]:size_to_gpu(storeptr)
        storeptr._gpu_data.cpu_meta.[sub_tblname]:index_to_gpu(storeptr)
      else
        assert(storeptr._gpu_data.store_valid.[ivname]:ok_GPU(),
               'INTERNAL: if size is valid on GPU the index should be too')
      end

      -- make sure that we have enough space regardless
      -- of whether we had to move this table over from the CPU
      var curr_size   = storeptr._gpu_data.cpu_meta.[sub_tblname]._size
      storeptr._gpu_data.cpu_meta.[sub_tblname]:resize(storeptr, curr_size)

      -- setup the visited tracking...
      storeptr._gpu_data.cpu_meta.[sub_tblname]._old_size = curr_size
      GPU.memzero( storeptr._gpu_data.cpu_meta.[sub_tblname]._visited,
                   curr_size/8 + 1 )

      -- since we wrote to the metadatablock, mark it for refresh onto the GPU
      storeptr._gpu_data.meta_valid = Valid_CPU_Only
      gpu_mem_log('set-valid-cpu',['meta-data (pre-merge)'])

      -- note that merging modifies the size on the GPU, invalidating the CPU
      storeptr._gpu_data.store_valid.[tblvalidname]
                                    ._size_valid  = Valid_GPU_Only
      storeptr._gpu_data.store_valid.[ivname]     = Valid_GPU_Only
      gpu_mem_log('set-valid-gpu',['size-'..Table:name()..' (pre-merge)'])
      gpu_mem_log('set-valid-gpu',['index-'..Index:fullname()..' (pre-merge)'])
      -- then we need to prepare to read-write the data in question
      escape for iField,Field in ipairs(fields) do
        local fname           = Field:name()
        emit(W:PrepareReadWrite(storeptr, tbltype, fname, on_gpu))
      end end
    end
  else
    -- on cpu, use the existing routine to do the pre-merge
    return quote
      -- make sure the table size and index are fresh
      storeptr._gpu_data.cpu_meta.[sub_tblname]:size_from_gpu(storeptr)
      storeptr._gpu_data.cpu_meta.[sub_tblname]:index_from_gpu(storeptr)
      [ W._main_wrap:PreMerge(storeptr, tbltype) ]

      storeptr._gpu_data.store_valid.[tblvalidname]
                                    ._size_valid  = Valid_CPU_Only
      storeptr._gpu_data.store_valid.[ivname]     = Valid_CPU_Only
      gpu_mem_log('set-valid-cpu',['size-'..Table:name()..' (pre-merge)'])
      gpu_mem_log('set-valid-cpu',['index-'..Index:fullname()..' (pre-merge)'])
      -- then we need to prepare to read-write the data in question
      escape for iField,Field in ipairs(fields) do
        local fname           = Field:name()
        emit(W:PrepareReadWrite(storeptr, tbltype, fname, on_gpu))
      end end
    end
  end
end


-------------------------------------------------------------------------------
--[[                        GPU Reduction Handling                         ]]--
-------------------------------------------------------------------------------

local function gpu_reduction_expr (op, typ, lptr, rval)
  assert(lptr:gettype():ispointer(), 'INTERNAL: expected pointer')
  if typ == T.float then
    if     op == '+'   then return `GPU.reduce_add_float([lptr], [rval])
    elseif op == '*'   then return `GPU.atomic_mul_float_SLOW([lptr], [rval])
    elseif op == 'min' then return `GPU.reduce_min_float([lptr], [rval])
    elseif op == 'max' then return `GPU.reduce_max_float([lptr], [rval])
    end

  elseif typ == T.double then
    if     op == '+'   then return `GPU.reduce_add_double([lptr],  [rval])
    elseif op == '*'   then return `GPU.atomic_mul_double_SLOW([lptr], [rval])
    elseif op == 'min' then return `GPU.reduce_min_double([lptr], [rval])
    elseif op == 'max' then return `GPU.reduce_max_double([lptr], [rval])
    end

  elseif typ == T.int32 then
    if     op == '+'   then return `GPU.reduce_add_int32([lptr], [rval])
    elseif op == '*'   then return `GPU.atomic_mul_int32_SLOW([lptr], [rval])
    elseif op == 'max' then return `GPU.reduce_max_int32([lptr], [rval])
    elseif op == 'min' then return `GPU.reduce_min_int32([lptr], [rval])
    end
  elseif typ == T.int64 then
    if     op == '+'   then return `GPU.reduce_add_int64([lptr], [rval])
    elseif op == '*'   then return `GPU.atomic_mul_int64_SLOW([lptr], [rval])
    elseif op == 'max' then return `GPU.reduce_max_int64([lptr], [rval])
    elseif op == 'min' then return `GPU.reduce_min_int64([lptr], [rval])
    end
  elseif typ == T.uint32 then
    if     op == '+'   then return `GPU.reduce_add_uint32([lptr], [rval])
    elseif op == '*'   then return `GPU.atomic_mul_uint32_SLOW([lptr], [rval])
    elseif op == 'max' then return `GPU.reduce_max_uint32([lptr], [rval])
    elseif op == 'min' then return `GPU.reduce_min_uint32([lptr], [rval])
    end
  elseif typ == T.uint64 then
    if     op == '+'   then return `GPU.reduce_add_uint64([lptr], [rval])
    elseif op == '*'   then return `GPU.atomic_mul_uint64_SLOW([lptr], [rval])
    elseif op == 'max' then return `GPU.reduce_max_uint64([lptr], [rval])
    elseif op == 'min' then return `GPU.reduce_min_uint64([lptr], [rval])
    end

  elseif typ == T.bool then
    assert(terralib.sizeof(T.bool:terratype()) == 1,
           'INTERNAL: Unexpected Boolean size...')
    if     op == 'and' then return quote
      var address = [lptr]
      var val     = [rval]
      if not val then -- save on memory traffic when op has no effect
        var aidx  = [uint64](address)
        var a     = 4*(aidx/4)
        var a_lo  = aidx % 4
        -- the mask prevents clobbering neighboring values
        -- with the 32-bit wide reduction operation
        var mask  = [uint32](0xFFFFFFFF) - ([uint32](0xFF) << (a_lo*8))
        GPU.reduce_and_b32([&uint32](a), mask)
      end end
    elseif op == 'or'  then return quote
      var address = [lptr]
      var val     = [rval]
      if val then -- save on memory traffic when op has no effect
        var aidx  = [uint64](address)
        var a     = 4*(aidx/4)
        var a_lo  = aidx % 4
        -- the mask prevents clobbering neighboring values
        -- with the 32-bit wide reduction operation
        var mask  = ([uint32](0xFF) << (a_lo*8))
        GPU.reduce_or_b32([&uint32](a), mask)
        -- NOTE: unsure if all 8 bits should be set or just the lowest?
      end end
    end
  end
  INTERNAL_ERR('unsupported reduction "'..op..'" on '..tostring(typ)..'; '..
               'this should be guarded against in the typechecker')
end

-------------------------------------------------------------------------------
--[[                     Gong Internal Data Interface                      ]]--
-------------------------------------------------------------------------------

function GWrapper:GPU_Struct()
  return self._c_cache[WRAPPER_ROOT].ctype
end
function GWrapper:GPU_Tables_Struct()
  return self._c_cache[WRAPPER_ROOT].GStore
end
function GWrapper:GPU_Globals_Struct()
  return self._c_cache[WRAPPER_ROOT].GGlobalFile
end

function GWrapper:Scan(name, storeptr, gpu_tblptr, gpu_globptr,
                       tbltype, rowsym, args, bodycode)
  local Table, main_tblname, sub_tblname, tblvalidname
                      = unpack_prepare_tbl(self, tbltype)

  local kargs         = newlist{ gpu_tblptr, gpu_globptr }
        kargs:insertall(args)
  local gpu_kernel = terra( [kargs] )
    -- lookup table sizes...
    var N             = [gpu_tblptr].[sub_tblname]._size
    var [rowsym]      = GPU.global_tid()
    if [rowsym] < N then
      [bodycode]
    end
  end
  gpu_kernel:setname(name..'_cudakernel')
  local loader = nil
  gpu_kernel, loader = GPU.simple_compile(gpu_kernel)
  self._cuda_loaders:insert(loader)

  -- code snippet that computes the number of threads to launch
  -- based on the storeptr values
  return quote
    -- figure out how many threads to launch
    var SIZE      = storeptr.[main_tblname]:size()
    -- extract the tblptr and globptr
    var tblptr    = storeptr._gpu_data.gpu_meta
    var globptr   = storeptr._gpu_data.gpu_globals
    -- launch the GPU kernel
    gpu_kernel(SIZE, tblptr, globptr, [args])
    GPU.sync()
  end
end

function GWrapper:ScanAB(name, storeptr, gpu_tblptr, gpu_globptr,
                         tbl0type, row0sym, tbl1type, row1sym,
                         args, bodycode)
  local Table0, main_tbl0name, sub_tbl0name, tbl0validname
                      = unpack_prepare_tbl(self, tbl0type)
  local Table1, main_tbl1name, sub_tbl1name, tbl1validname
                      = unpack_prepare_tbl(self, tbl1type)

  local kargs         = newlist{ gpu_tblptr, gpu_globptr }
        kargs:insertall(args)
  local gpu_kernel = terra( [kargs] )
    -- lookup table sizes...
    var N0            = gpu_tblptr.[sub_tbl0name]._size
    var N0_hi         = (N0+7)/8
    var N1            = gpu_tblptr.[sub_tbl1name]._size
    var N1_hi         = (N1+7)/8
    -- block thread ids slightly
    var lin_id        = GPU.global_tid()
    var lin_hi,lin_lo = lin_id / 64, lin_id % 64
    var A_hi, B_hi    = lin_hi % N0_hi, lin_hi / N0_hi
    var A_lo, B_lo    = lin_lo % 8, lin_lo / 8
    var [row0sym]     = A_hi * 8 + A_lo
    var [row1sym]     = B_hi * 8 + B_lo
    -- make sure to guard against out-of-range ids
    if [row0sym] < N0 and [row1sym] < N1 then
      [bodycode]
    end
  end
  gpu_kernel:setname(name..'_cudakernel')
  local loader = nil
  gpu_kernel, loader = GPU.simple_compile(gpu_kernel)
  self._cuda_loaders:insert(loader)

  -- code snippet that computes the number of threads to launch
  -- based on the storeptr values
  return quote
    -- figure out how many threads to launch
    var SIZE0     = storeptr.[main_tbl0name]:size()
    var SIZE1     = storeptr.[main_tbl1name]:size()
    assert(SIZE0 <= 65530, 'Cannot launch double-scan on 64k+ rows')
    assert(SIZE1 <= 65530, 'Cannot launch double-scan on 64k+ rows')
    var SZ0_hi    = (SIZE0+7)/8
    var SZ1_hi    = (SIZE1+7)/8
    var N_launch  = SZ0_hi * SZ1_hi * 64
    -- extract the tblptr and globptr
    var tblptr    = storeptr._gpu_data.gpu_meta
    var globptr   = storeptr._gpu_data.gpu_globals
    -- launch the GPU kernel
    gpu_kernel(N_launch, tblptr, globptr, [args])
    GPU.sync()
  end
end

function GWrapper:ScanAA(name, storeptr, gpu_tblptr, gpu_globptr,
                         tbltype, row0sym, row1sym, args, bodycode
)
  local Table, main_tblname, sub_tblname, tblvalidname
                      = unpack_prepare_tbl(self, tbltype)

  local kargs         = newlist{ gpu_tblptr, gpu_globptr }
        kargs:insertall(args)
  local gpu_kernel = terra( [kargs] )
    -- lookup table size...
    var N             = gpu_tblptr.[sub_tblname]._size
    var N_hi          = (N+7)/8
    -- block thread ids slightly
    var lin_id        = GPU.global_tid()
    var lin_hi,lin_lo = lin_id / 64, lin_id % 64
    var A_hi, B_hi    = lin_hi % N_hi, lin_hi / N_hi
    var A_lo, B_lo    = lin_lo % 8, lin_lo / 8
    var [row0sym]     = A_hi * 8 + A_lo
    var [row1sym]     = B_hi * 8 + B_lo
    -- make sure to guard against out-of-range ids
    if [row0sym] < N and [row1sym] < N then
      -- TODO: tighten launch pattern here...
      if [row1sym] >= [row0sym] then
        [bodycode]
      end
    end
  end
  gpu_kernel:setname(name..'_cudakernel')
  local loader = nil
  gpu_kernel, loader = GPU.simple_compile(gpu_kernel)
  self._cuda_loaders:insert(loader)

  -- code snippet that computes the number of threads to launch
  -- based on the storeptr values
  return quote
    --C.printf("WARNING TODO: need to tighten symmetric launch...\n")
    -- figure out how many threads to launch
    var SIZE      = storeptr.[main_tblname]:size()
    assert(SIZE <= 65530, 'Cannot launch double-scan on 64k+ rows')
    var SZ_hi     = (SIZE+7)/8
    var N_launch  = SZ_hi * SZ_hi * 64
    -- extract the tblptr and globptr
    var tblptr    = storeptr._gpu_data.gpu_meta
    var globptr   = storeptr._gpu_data.gpu_globals
    -- launch the GPU kernel
    gpu_kernel(N_launch, tblptr, globptr, [args])
    GPU.sync()
  end
end

function GWrapper:LoopGen(name, storeptr, is_self_join,
                          gpu_tblptr, gpu_globptr,
                          traversal, row0sym, row1sym, args, bodycode)
  local W               = self
  local MainW           = W._main_wrap
  local IndexL, IndexR  = traversal:left(), traversal:right()
  local inameL, inameR  = MainW._c_cache[IndexL].name,
                          MainW._c_cache[IndexR].name
  local idxptrL         = `&(storeptr.[inameL])
  local idxptrR         = `&(storeptr.[inameR])

  local loopgen   = traversal:_INTERNAL_LoopGen( MainW:GetAccAPI(), true )
  return loopgen( name, storeptr, is_self_join, gpu_tblptr, gpu_globptr,
                  idxptrL, idxptrR, row0sym, row1sym, args, bodycode )
end

---------------------------------------

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

local function prelude_lookup(GW, gpuptr, tbltype, rowsym, path)
  assert(#path >= 1 and is_id_str(path[1]),
         'INTERNAL: Must have a field to access')
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)
  local Field           = assert(Table:fields(path[1]),
                                 'INTERNAL: field lookup failed')
  local fname           = GW._c_cache[Field].name
  path                  = path:sublist(2)

  local fptr_expr       = `([gpuptr].[sub_tblname].[fname])[rowsym]
  local val, stmts, typ = apply_path(fptr_expr, Field:type(), path)

  return val, stmts, typ, Field
end



function GWrapper:Read(gpuptr, tbltype, rowsym, path)
  local read_val, stmts     = prelude_lookup(self, gpuptr,
                                             tbltype, rowsym, path)
  if #stmts == 0 then
    return read_val
  else
    return quote [stmts] in [read_val] end
  end
end

function GWrapper:Write(gpuptr, tbltype, rowsym, path, rval)
  local lookup, stmts       = prelude_lookup(self, gpuptr,
                                             tbltype, rowsym, path)

  return quote [stmts]; [lookup] = [rval] end
end

function GWrapper:Reduce(gpuptr, tbltype, op, rowsym, path, rval)
  local lookup, stmts, typ  = prelude_lookup(self, gpuptr,
                                             tbltype, rowsym, path)

  if typ:is_tensor() then
    local btyp              = typ:basetype()
    local Nd                = typ._n_entries
    return quote
      [stmts]
      var rhs = [rval]
      for k=0,Nd do
        [ gpu_reduction_expr( op, btyp, `&([lookup].d[k]), (`rhs.d[k]) ) ]
      end
    end
  else
    return quote
      [stmts]
      [ gpu_reduction_expr( op, typ, (`&[lookup]), rval ) ]
    end
  end
end

local function prelude_global(self, gpu_globptr, Global, path)
  local globname            = self._c_cache[Global].name
  local globvar_expr        = `gpu_globptr.[globname]
  local val, stmts, typ     = apply_path(globvar_expr, Global:type(), path)

  return val, stmts, typ, Global
end

function GWrapper:ReadGlobal(gpu_globptr, glob, path)
  local read_val, stmts     = prelude_global(self, gpu_globptr, glob, path)

  if #stmts == 0 then
    return read_val
  else
    return quote [stmts] in [read_val] end
  end
end

function GWrapper:ReduceGlobal(gpu_globptr, glob, op, path, rval)
  local gvar, stmts, typ    = prelude_global(self, gpu_globptr, glob, path)
  local REDUCE_OP           = CodeGen.REDUCE_OP

  print("TODO: Should Improve global reduction implementation")

  if typ:is_tensor() then
    local btyp              = typ:basetype()
    local Nd                = typ._n_entries
    return quote
      [stmts]
      var rhs = [rval]
      for k=0,Nd do
        [ gpu_reduction_expr( op, btyp, `&([gvar].d[k]), (`rhs.d[k]) ) ]
      end
    end
  else
    return quote
      [stmts]
      [ gpu_reduction_expr( op, typ, (`&[gvar]), rval ) ]
    end
  end
end

function GWrapper:WriteRow(gpuptr, tbltype, rowsym, vals)
  local GW              = self
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)
  local fields          = Table:_INTERNAL_GPU_fields()

  local stmts           = newlist()
  -- write values into the fields at that row
  for iField,Field in ipairs(fields) do
    local fname       = GW._c_cache[Field].name
    local valexpr     = vals[iField]
    stmts:insert(quote
      (gpuptr.[sub_tblname].[fname])[rowsym] = [valexpr]
    end)
  end
  return quote [stmts] end
end

function GWrapper:Insert(gpuptr, tbltype, vals)
  local GW              = self
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)

  return quote
    var row             = [ GW:_INTERNAL_NewRow(gpuptr, tbltype) ]
    -- and write values into the fields at that row
    [ GW:WriteRow(gpuptr, tbltype, row, vals) ]
  end --in row end -- might as well return the new row value
end


function GWrapper:MergeLookup(
  gpuptr, tbltype,
  row, key0, key1, body, else_vals
)
  local GW              = self
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)

  local dst_indices     = self._c_cache[Table].dst_indices
  assert(#dst_indices == 1, 'INTERNAL: expect exactly one index')
  local Index           = dst_indices[1]
  local iname           = self._c_cache[Index].name
  assert(#Index:srcs() == 2, "INTERNAL: expect two keys for index")
  local f0,f1           = unpack(Index:srcs())
  local f1name          = self._c_cache[f1].name

  return quote
    var f1ptr           = gpuptr.[sub_tblname].[f1name]
    var success, rowid  = gpuptr.[iname]:lookup(f1ptr, [key0], [key1])
    if success then
      var [row]         = rowid
      [ GW:_INTERNAL_visit(gpuptr, tbltype, row) ]
      [body]
    else escape if else_vals then emit quote
      var row           = [ GW:_INTERNAL_NewRow(gpuptr, tbltype) ]
      [ GW:WriteRow(gpuptr, tbltype, row, else_vals) ]
    end end end end
  end
end

function GWrapper:KeepRow(gpuptr, tbltype, row)
  local GW              = self
  return GW:_INTERNAL_visit(gpuptr, tbltype, row)
end

function GWrapper:_INTERNAL_visit(gpuptr, tbltype, row)
  local GW              = self
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)
  return quote
    var r               = [uint32](row)
    var i               = r >> 5 -- r/32
    var mask            = 1 << (r and 0x1F) -- 1 << (r%32)
    GPU.reduce_or_b32( &(gpuptr.[sub_tblname]._visited[i]), mask )
  end
end
function GWrapper:_INTERNAL_NewRow(gpuptr, tbltype)
  local GW              = self
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)

  return quote
    -- allocate a row for the insertion
    var MAX_SIZE        = gpuptr.[sub_tblname]._alloc_size
    var tid             = GPU.global_tid()
    var row             = GPU.reserve_idx(tid, &(gpuptr.[sub_tblname]._size))
    -- Overflow Error Condition
    if row >= MAX_SIZE then GPU.trap() end
  in row end
end
function GWrapper:_INTERNAL_was_visited(gpuptr, tbltype, row)
  local GW              = self
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)
  return quote
    var r               = [uint32](row)
    var i               = r >> 5 -- r/32
    var mask            = 1 << (r and 0x1F) -- 1 << (r%32)
    var word            = gpuptr.[sub_tblname]._visited[i]
  in (mask and word) ~= 0 end
end


function GWrapper:PostMerge(storeptr,gpuptr,globptr, tbltype, rm_var, rm_body)
  local GW              = self
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)
  local fields          = Table:_INTERNAL_GPU_fields()

  local dst_indices     = self._c_cache[Table].dst_indices
  assert(#dst_indices == 1, 'INTERNAL: expect exactly one index')
  assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')
  local Index           = dst_indices[1]
  local iname           = GW._c_cache[Index].name
  local f0,f1           = unpack(Table:primary_key())
  local f0name          = self._c_cache[f0].name
  local f1name          = self._c_cache[f1].name
  local T0,T1           = f0:type():table(), f1:type():table()
  local t0name, t1name  = GW._c_cache[T0].name, GW._c_cache[T1].name

  local args            = nil
  local size_bound      = symbol(uint64,  'size_bound')
  local sortkey_in      = symbol(&uint64, 'sortkey_in')
  local sortkey_out     = symbol(&uint64, 'sortkey_out')
  local idx_in          = symbol(&uint32, 'idx_in')
  local idx_out         = symbol(&uint32, 'idx_out')

  -- Steps
  --  1)  map over the table, decide removal, and generate sort data
  --  2)  execute the actual sort
  --  3)  unpack keys back to arrays
  --  4)  regenerate segment index
  --  5)  shuffle all non-key data arrays

  -- (Step 1)
  args = newlist { size_bound, gpuptr, globptr, sortkey_in, idx_in }
  local terra post_merge_pack( [args] )
    var N               = size_bound
    var Old_N           = gpuptr.[sub_tblname]._old_size
    var N_k0, N_k1      = gpuptr.[t0name]:size(), gpuptr.[t1name]:size()
    var max_bit_0       = [uint32](GPU.ceil( GPU.log2(N_k0) ))
    var max_bit_1       = [uint32](GPU.ceil( GPU.log2(N_k1) ))
    -- plus one to make sure there are values above any legit value
    var total_bits      = max_bit_0 + max_bit_1 + 1

    var tid             = [uint32](GPU.global_tid())
    -- processing of old rows
    if tid < Old_N then
      -- check whether we visited this old row
      var VISITED       = [ GW:_INTERNAL_was_visited(gpuptr, tbltype, tid) ]
      -- process rows to remove
      escape if rm_var then emit quote
        if not VISITED then
          var [rm_var]  = tid
          [rm_body]
          VISITED       = [ GW:_INTERNAL_was_visited(gpuptr, tbltype, tid) ]
        end
      end end end

      -- give items we want removed a MAX key value so they get sorted last
      var k01 : uint64  = 0xFFFFFFFFFFFFFFFFULL
      if VISITED then
        var k0          = [uint64]( (gpuptr.[sub_tblname].[f0name])[tid] )
        var k1          = [uint64]( (gpuptr.[sub_tblname].[f1name])[tid] )
        k01             = (k0 << max_bit_1) or k1
      else
        -- make sure to decrease the size to account for truly dead rows
        GPU.warp_dec_32(tid, &(gpuptr.[sub_tblname]._size))
      end
      sortkey_in[tid]   = k01
      idx_in[tid]       = tid

    -- processing of new rows
    elseif tid < N then
      var k0            = [uint64]( (gpuptr.[sub_tblname].[f0name])[tid] )
      var k1            = [uint64]( (gpuptr.[sub_tblname].[f1name])[tid] )
      var k01           = (k0 << max_bit_1) or k1
      sortkey_in[tid]   = k01
      idx_in[tid]       = tid
    end
  end

  local post_merge_pack,
        post_merge_pack_init    = GPU.simple_compile(post_merge_pack)
  self._cuda_loaders:insert(post_merge_pack_init)

  return quote
    -- make sure we have the right values to do this work with
    storeptr:meta_from_gpu()

    var [gpuptr]        = storeptr._gpu_data.gpu_meta
    var [globptr]       = storeptr._gpu_data.gpu_globals
    var [size_bound]    = storeptr._gpu_data.cpu_meta.[sub_tblname]._size   

    var align           = 64  -- 64*4-byte alignment
    var sortspace       = align*(size_bound/align + 1)
    var scratchptr      = gmalloc_ptr(uint32, 6*sortspace)
    var [sortkey_in]    = [&uint64](scratchptr + 0*sortspace)
    var [sortkey_out]   = [&uint64](scratchptr + 2*sortspace)
    var [idx_in]        = [&uint32](scratchptr + 4*sortspace)
    var [idx_out]       = [&uint32](scratchptr + 5*sortspace)

    -- cpu-side max bit computation
    var N_k0            = storeptr._gpu_data.cpu_meta.[t0name]:size()
    var N_k1            = storeptr._gpu_data.cpu_meta.[t1name]:size()
    var max_bit_0       = [uint32](C.ceil( C.log(N_k0)/C.log(2) ))
    var max_bit_1       = [uint32](C.ceil( C.log(N_k1)/C.log(2) ))
    var max_bits        = max_bit_0 + max_bit_1 + 1

    -- (Step 1)
    post_merge_pack(size_bound, size_bound, gpuptr,globptr, sortkey_in, idx_in)
    -- (Step 2)
    GPU.sort64(size_bound, max_bits, sortkey_in, sortkey_out, idx_in, idx_out)

    -- get the new size (computed in step 1) back from the GPU
    storeptr._gpu_data.meta_valid = Valid_GPU_Only
    gpu_mem_log('set-valid-gpu',['meta-data (post-merge)'])
    storeptr:meta_from_gpu()
    size_bound          = storeptr._gpu_data.cpu_meta.[sub_tblname]._size

    -- (Steps 3,4,5)
    [ GW:_INTERNAL_common_index_rebuild( storeptr, tbltype, gpuptr,
                                         size_bound, sortkey_out, idx_out ) ]

    -- clean up the scratch space
    gfree_ptr(scratchptr)
    GPU.sync()
  end
end

function GWrapper:PostEmit(storeptr, tbltype)
  local GW              = self
  local GStore          = GW:GPU_Tables_Struct()
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)
  local fields          = Table:_INTERNAL_GPU_fields()

  -- we don't need to do this if
  -- we are just emitting into an unindexed table
  if not Table._is_live then return quote end end


  local dst_indices     = self._c_cache[Table].dst_indices
  assert(#dst_indices == 1, 'INTERNAL: expect exactly one index')
  assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')
  local Index           = dst_indices[1]
  local iname           = GW._c_cache[Index].name
  local f0,f1           = unpack(Table:primary_key())
  local f0name          = self._c_cache[f0].name
  local f1name          = self._c_cache[f1].name
  local T0,T1           = f0:type():table(), f1:type():table()
  local t0name, t1name  = GW._c_cache[T0].name, GW._c_cache[T1].name

  local args            = nil
  local gpuptr          = symbol(&GStore, 'gpuptr')
  local size_bound      = symbol(uint64,  'size_bound')
  local sortkey_in      = symbol(&uint64, 'sortkey_in')
  local sortkey_out     = symbol(&uint64, 'sortkey_out')
  local idx_in          = symbol(&uint32, 'idx_in')
  local idx_out         = symbol(&uint32, 'idx_out')

  -- Steps
  --  1)  generate sort data
  --  2)  execute the actual sort
  --  3)  unpack keys back to arrays
  --  4)  regenerate segment index
  --  5)  shuffle all non-key data arrays

  -- (Step 1)
  local pack_cached     = GW._c_cache[Index]._cached_emit_pack_kernel
  if not pack_cached then
    args = newlist { size_bound, gpuptr, sortkey_in, idx_in }
    local terra emit_pack( [args] )
      var N             = size_bound
      var N_k0, N_k1    = gpuptr.[t0name]:size(), gpuptr.[t1name]:size()
      var max_bit_0     = [uint32](GPU.ceil( GPU.log2(N_k0) ))
      var max_bit_1     = [uint32](GPU.ceil( GPU.log2(N_k1) ))
      -- plus one to make sure there are values above any legit value
      var total_bits    = max_bit_0 + max_bit_1 + 1

      var tid           = [uint32](GPU.global_tid())
      if tid < N then
        var k0          = [uint64]( (gpuptr.[sub_tblname].[f0name])[tid] )
        var k1          = [uint64]( (gpuptr.[sub_tblname].[f1name])[tid] )
        var k01         = (k0 << max_bit_1) or k1
        sortkey_in[tid] = k01
        idx_in[tid]     = tid
      end
    end
    local emit_pack, emit_pack_init = GPU.simple_compile(emit_pack)
    self._cuda_loaders:insert(emit_pack_init)
    GW._c_cache[Index]._cached_emit_pack_kernel = emit_pack
    pack_cached         = emit_pack
  end
  local emit_pack       = pack_cached

  local post_merge_pack,
        post_merge_pack_init    = GPU.simple_compile(post_merge_pack)
  self._cuda_loaders:insert(post_merge_pack_init)


  return quote
    -- make sure we have the right values to do this work with
    storeptr:meta_from_gpu()

    var [gpuptr]        = storeptr._gpu_data.gpu_meta
    var [size_bound]    = storeptr._gpu_data.cpu_meta.[sub_tblname]._size   

    var align           = 64  -- 64*4-byte alignment
    var sortspace       = align*(size_bound/align + 1)
    var scratchptr      = gmalloc_ptr(uint32, 6*sortspace)
    var [sortkey_in]    = [&uint64](scratchptr + 0*sortspace)
    var [sortkey_out]   = [&uint64](scratchptr + 2*sortspace)
    var [idx_in]        = [&uint32](scratchptr + 4*sortspace)
    var [idx_out]       = [&uint32](scratchptr + 5*sortspace)

    -- cpu-side max bit computation
    var N_k0            = storeptr._gpu_data.cpu_meta.[t0name]:size()
    var N_k1            = storeptr._gpu_data.cpu_meta.[t1name]:size()
    var max_bit_0       = [uint32](C.ceil( C.log(N_k0)/C.log(2) ))
    var max_bit_1       = [uint32](C.ceil( C.log(N_k1)/C.log(2) ))
    var max_bits        = max_bit_0 + max_bit_1 + 1

    -- (Step 1)
    emit_pack(size_bound, size_bound, gpuptr, sortkey_in, idx_in)
    -- (Step 2)
    GPU.sort64(size_bound, max_bits, sortkey_in, sortkey_out, idx_in, idx_out)

    -- unlike the merging case, we don't need to eliminate dead rows
    -- so we don't need to do any size/meta-data-validity updates

    -- (Steps 3,4,5)
    [ GW:_INTERNAL_common_index_rebuild( storeptr, tbltype, gpuptr,
                                         size_bound, sortkey_out, idx_out ) ]

    -- clean up the scratch space
    gfree_ptr(scratchptr)
    GPU.sync()
  end
end

function GWrapper:_INTERNAL_common_index_rebuild(
  storeptr, tbltype,
  gpuptr, size_bound, sortkey_out, idx_out
)
  local GW              = self
  local GStore          = GW:GPU_Tables_Struct()
  local Table, main_tblname, sub_tblname, tblvalidname
                        = unpack_prepare_tbl(GW, tbltype)
  local fields          = Table:_INTERNAL_GPU_fields()

  local dst_indices     = self._c_cache[Table].dst_indices
  assert(#dst_indices == 1, 'INTERNAL: expect exactly one index')
  assert(#Table:primary_key() == 2, 'INTERNAL: expect 2 primary keys')
  local Index           = dst_indices[1]
  local iname           = GW._c_cache[Index].name
  local ivname          = GW._cv_cache[Index].name
  local f0,f1           = unpack(Table:primary_key())
  local f0name          = self._c_cache[f0].name
  local f1name          = self._c_cache[f1].name
  local T0,T1           = f0:type():table(), f1:type():table()
  local t0name, t1name  = GW._c_cache[T0].name, GW._c_cache[T1].name

  -- Steps
  --  1)  [varies in details] generate sort data
  --  2)  execute the actual sort
  --  3)  unpack keys back to arrays
  --  4)  regenerate segment index
  --  5)  shuffle all non-key data arrays

  -- (Step 3)
  local unpack_cached   = GW._c_cache[Index]._cached_unpack_kernel
  if not unpack_cached then
    local args = newlist { gpuptr, sortkey_out }
    local terra common_unpack( [args] )
      -- note that we can now focus on only the live rows
      var N             = gpuptr.[sub_tblname]:size()
      -- bit packing information
      var N_k0, N_k1    = gpuptr.[t0name]:size(), gpuptr.[t1name]:size()
      var max_bit_0     = [uint32](GPU.ceil( GPU.log2(N_k0) ))
      var max_bit_1     = [uint32](GPU.ceil( GPU.log2(N_k1) ))

      var tid           = [uint32](GPU.global_tid())
      if tid < N then
        var k01         = sortkey_out[tid]
        var mask        = (1 << max_bit_1) - 1
        var k0          = [f0:type():terratype()](k01 >> max_bit_1)
        var k1          = [f1:type():terratype()](mask and k01)
        (gpuptr.[sub_tblname].[f0name])[tid]  = k0
        (gpuptr.[sub_tblname].[f1name])[tid]  = k1
      end
    end
    local common_unpack,
          common_unpack_init  = GPU.simple_compile(common_unpack)
    self._cuda_loaders:insert(common_unpack_init)
    GW._c_cache[Index]._cached_unpack_kernel = common_unpack
    unpack_cached       = common_unpack
  end
  local common_unpack   = unpack_cached

  return quote 
    var n_alloc = storeptr._gpu_data.cpu_meta.[sub_tblname]._alloc_size

    -- (Step 3)
    common_unpack(size_bound, gpuptr, sortkey_out)
    -- (Step 4)
    storeptr._gpu_data.cpu_meta.[iname]:rebuild_already_sorted(storeptr)
    -- (Step 5)
    escape for iField,Field in ipairs(fields) do
      if Field ~= f0 and Field ~= f1 then
        local fname     = GW._c_cache[Field].name
        local ftype     = Field:type():terratype()
        emit quote
          var old_data  = storeptr._gpu_data.cpu_meta.[sub_tblname].[fname]
          var new_data  = gmalloc_ptr(ftype, n_alloc)
          GPU.datashuffle(size_bound, terralib.sizeof(ftype),
                          idx_out, old_data, new_data)
          storeptr._gpu_data.cpu_meta.[sub_tblname].[fname] = new_data
          gfree_ptr(old_data)
          gpu_mem_log('index-rebuild-shuffle',
                      ['index-sort-field-'..Field:fullname()])
        end
      end
    end end
    -- the index should now be valid, but only on the GPU
    storeptr._gpu_data.store_valid.[ivname]         = Valid_GPU_Only
    gpu_mem_log('set-valid-gpu',['index-'..Index:fullname()..' (post-merge)'])
    -- the meta data is only valid on the CPU now that we've
    -- swapped the field pointers around
    storeptr._gpu_data.meta_valid = Valid_CPU_Only
    gpu_mem_log('set-valid-cpu',['meta-data (post-merge)'])
  end
end











