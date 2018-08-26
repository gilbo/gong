--import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.gpu_datastore"] = Exports

local PARAMETER     = (require 'gong.src.params').get_param
local verbosity     = require('gong.src.verbosity').get_verbosity()

if not PARAMETER('GPU_ENABLED') then return end

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

--local CodeGen       = require 'gong.src.codegen'

-----------------------------------------

local C             = require 'gong.src.c'
local assert        = C.assert

local StdContainers = require 'gong.src.stdcontainers'
local vector        = StdContainers.vector

local GPU           = require 'gong.src.gpu_util'

local newlist       = terralib.newlist


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
    }

  These decompose over the schema as

    GTable_i {
      alloc_size      : Size_Type_i     -- allocated space
      size            : Size_Type_i     -- used space
      <col_name_0>    : &<type_0>
      <col_name_1>    : &<type_1>
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

    GTableValid.entries:insertall {
      { '_size_valid', ValidLocation }
    }

    local fields            = Table:_INTERNAL_fields()
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
    W._c_cache[Table]   = { ctype = GTable,      name = '_'..TableName }
    W._cv_cache[Table]  = { ctype = GTableValid, name = '_'..TableValidName }
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

  -- Initialization and Destruction
  local MIN_INIT_SIZE = 8
  terra GMetaData:init()
    var this                = self

    -- to start, fill out the cpu version of the mirror structures
    escape for iTable,Table in ipairs(W._tables) do
      local tblname           = W._c_cache[Table].name
      local tblvalidname      = W._cv_cache[Table].name

      -- we start with everything valid, but empty
      emit quote
        this.cpu_meta.[tblname]._alloc_size         = MIN_INIT_SIZE
        this.cpu_meta.[tblname]._size               = 0
        this.store_valid.[tblvalidname]._size_valid = Valid_All
      end

      local fields            = Table:_INTERNAL_fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        local fvname          = W._cv_cache[Field].name
        local ftype           = Field:type():terratype()
        emit quote
          this.cpu_meta.[tblname].[fname] = gmalloc_ptr(ftype, MIN_INIT_SIZE)
          this.store_valid.[tblvalidname].[fvname] = Valid_All
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
    end end

    -- Then, copy over the mirror structures to the GPU
    this.gpu_meta       = gmalloc_ptr(GStore, 1)
    this.gpu_globals    = gmalloc_ptr(GGlobalFile, 1)
    GPU.memcpy_to_gpu(this.gpu_meta, &(this.cpu_meta),
                      sizeof(GStore))
    GPU.memcpy_to_gpu(this.gpu_globals, &(this.cpu_globals),
                      sizeof(GGlobalFile))
    self.meta_valid     = Valid_All
    self.globals_valid  = Valid_All
  end
  terra GMetaData:destroy()
    var this                  = self
    escape for iTable,Table in ipairs(W._tables) do
      local tblname           = W._c_cache[Table].name
      local fields            = Table:_INTERNAL_fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        emit quote
          gfree_ptr( this.cpu_meta.[tblname].[fname] )
          this.cpu_meta.[tblname].[fname] = nil -- unnecessary defensiveness
        end
      end
    end end

    gfree_ptr(this.gpu_globals)
    gfree_ptr(this.gpu_meta)
  end
end

local function Install_Data_Extensions(W, MainW, MAIN_WRAP_ROOT)
  W._main_wrap              = MainW
  local CStore              = MainW._c_cache[MAIN_WRAP_ROOT].ctype

  local GMetaData           = W._c_cache[WRAPPER_ROOT].ctype
  local GStore              = W._c_cache[WRAPPER_ROOT].GStore
  local GValidStore         = W._c_cache[WRAPPER_ROOT].GValidStore
  local GGlobalFile         = W._c_cache[WRAPPER_ROOT].GGlobalFile

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

      local fields            = Table:_INTERNAL_fields()
      for iField,Field in ipairs(fields) do
        local fvname          = W._cv_cache[Field].name
        emit quote
          var fv = this.store_valid.[tblvalidname].[fvname]
          C.printf("    %s : %s\n", fv:string(), fvname)
        end
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

      local fields            = Table:_INTERNAL_fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        emit quote
          var fp = this.cpu_meta.[tblname].[fname]
          C.printf("    %p : %s\n", fp, fname)
        end
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
    self._gpu_data.meta_valid = Valid_All
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
    self._gpu_data.meta_valid = Valid_All
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
    self._gpu_data.globals_valid = Valid_All
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
    self._gpu_data.globals_valid = Valid_All

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

    local fields            = Table:_INTERNAL_fields()

    GTable.methods.size     = macro(function(self) return `self._size end)

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

      -- now, copy the size over from the main data
      var oldalloc  = storeptr._gpu_data.cpu_meta.[sub_tblname]._alloc_size
      var oldsize   = storeptr._gpu_data.cpu_meta.[sub_tblname]._size
      var newsize   = storeptr.[main_tblname]._size
      -- work out new allocation size to be tight for now
      var newalloc  = newsize
      -- commit new sizes to the metadata block
      storeptr._gpu_data.cpu_meta.[sub_tblname]._alloc_size   = newalloc
      storeptr._gpu_data.cpu_meta.[sub_tblname]._size         = newsize

      -- re-allocate the gpu arrays at the new size
      -- and sanity check the field data
      escape for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        local fvname          = W._cv_cache[Field].name
        local ftype           = Field:type():terratype()

        emit quote
          gfree_ptr(storeptr._gpu_data.cpu_meta.[sub_tblname].[fname])
          storeptr._gpu_data.cpu_meta.[sub_tblname]
                                     .[fname] = gmalloc_ptr(ftype, newsize)
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
      storeptr.[main_tblname]:resize(storeptr, newsize)

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
    end
  end
end

local function NewSubWrapper(args)
  assert(#args.indices == 0,
         'INTERNAL: GPU does not support indices currently')

  local W = setmetatable({
    _tables         = args.tables:copy(),
    _globals        = args.globals:copy(),
    _c_cache        = {},
    _cv_cache       = {}, -- for the valid stuff
    _func_cache     = {},
    _cuda_loaders   = newlist{ GPU.load_init_rand },
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




-------------------------------------------------------------------------------
--[[        Wrapper Helper Functions; INTERNAL to DataStore modules        ]]--
-------------------------------------------------------------------------------

function GWrapper:PrepareReadWriteGlobals(storeptr, on_gpu)
  if on_gpu then
    return quote  storeptr:globals_to_gpu()
                  storeptr._gpu_data.globals_valid = Valid_GPU_Only   end
  else
    return quote  storeptr:globals_from_gpu()
                  storeptr._gpu_data.globals_valid = Valid_CPU_Only   end
  end
end
function GWrapper:PrepareReadGlobals(storeptr, on_gpu)
  if on_gpu then
    return quote  storeptr:globals_to_gpu()   end
  else
    return quote  storeptr:globals_from_gpu() end
  end
end

function GWrapper:PrepareOverWrite(storeptr, tbltype, field_name, on_gpu)
  local W         = self
  local metatbl, fbytes, fvalid, fc_ptr, fg_ptr
                  = prepare_prelude(W, storeptr, tbltype, field_name)

  -- no need to copy; only invalidate
  if on_gpu then
    return quote  [metatbl]:size_to_gpu(storeptr)
                  fvalid = Valid_GPU_Only end
  else
    return quote  [metatbl]:size_from_gpu(storeptr)
                  fvalid = Valid_CPU_Only end
  end
end

function GWrapper:PrepareReadWrite(storeptr, tbltype, field_name, on_gpu)
  local W         = self
  local metatbl, fbytes, fvalid, fc_ptr, fg_ptr
                  = prepare_prelude(W, storeptr, tbltype, field_name)

  -- need to copy & invalidate
  if on_gpu then
    return quote  [metatbl]:size_to_gpu(storeptr)
                  if not fvalid:ok_GPU() then
                    assert(fvalid:ok_CPU(), -- sanity check
                           'INTERNAL: expect field valid on CPU')
                    GPU.memcpy_to_gpu(fg_ptr, fc_ptr, fbytes)
                  end
                  fvalid = Valid_GPU_Only end
  else
    return quote  [metatbl]:size_from_gpu(storeptr)
                  if not fvalid:ok_CPU() then
                    assert(fvalid:ok_GPU(), -- sanity check
                           'INTERNAL: expect field valid on GPU')
                    GPU.memcpy_from_gpu(fc_ptr, fg_ptr, fbytes)
                  end
                  fvalid = Valid_CPU_Only end
  end
end

function GWrapper:PrepareRead(storeptr, tbltype, field_name, on_gpu)
  local W         = self
  local metatbl, fbytes, fvalid, fc_ptr, fg_ptr
                  = prepare_prelude(W, storeptr, tbltype, field_name)

  -- need to copy; not invalidate
  if on_gpu then
    return quote  [metatbl]:size_to_gpu(storeptr)
                  if not fvalid:ok_GPU() then
                    assert(fvalid:ok_CPU(), -- sanity check
                           'INTERNAL: expect field valid on CPU')
                    GPU.memcpy_to_gpu(fg_ptr, fc_ptr, fbytes)
                    fvalid = Valid_All
                  end end
  else
    return quote  [metatbl]:size_from_gpu(storeptr)
                  if not fvalid:ok_CPU() then
                    assert(fvalid:ok_GPU(), -- sanity check
                           'INTERNAL: expect field valid on GPU')
                    GPU.memcpy_from_gpu(fc_ptr, fg_ptr, fbytes)
                    fvalid = Valid_All
                  end end
  end
  -- no need to invalidate; only copy
end

function GWrapper:PrepareFinal(storeptr, on_gpu, gpu_size_modified)
  if on_gpu then
    local meta_refresh = quote storeptr:meta_to_gpu() end
    -- if the size is being modified, make sure to invalidate the
    -- CPU-side metadata to force a refresh of it when next needed
    local cpu_meta_invalidate = quote
      storeptr._gpu_data.meta_valid = Valid_GPU_Only
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
  local fields        = Table:_INTERNAL_fields()

  if on_gpu then
    local SizeT       = Table:_INTERNAL_terra_size_type()
    local sztbl       = Table._size_func
    local szexpr      = (`[SizeT]([sztbl.offset]))
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
    return quote
      -- make sure that the metadata block is fresh before we
      -- start reading from or writing to it
      storeptr:meta_from_gpu()
      var clear_to_size       = [szexpr]

      storeptr._gpu_data.cpu_meta.[sub_tblname]._alloc_size = clear_to_size
      storeptr._gpu_data.cpu_meta.[sub_tblname]._size       = 0
      escape for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        local ftype           = Field:type():terratype()
        emit quote
          gfree_ptr(storeptr._gpu_data.cpu_meta.[sub_tblname].[fname])
          storeptr._gpu_data.cpu_meta.[sub_tblname]
                            .[fname] = gmalloc_ptr(ftype, clear_to_size)
        end
      end end
      -- since we wrote to the metadatablock, mark it for refresh onto the GPU
      storeptr._gpu_data.meta_valid = Valid_CPU_Only

      -- and then invalidate the CPU data
      -- But make sure to also validate the GPU data fields...

      storeptr._gpu_data.store_valid.[tblvalidname]
                                    ._size_valid = Valid_GPU_Only
      escape for iField,Field in ipairs(fields) do
        local fvname          = W._cv_cache[Field].name
        emit quote
          storeptr._gpu_data.store_valid.[tblvalidname]
                                        .[fvname] = Valid_GPU_Only
        end
      end end
    end
  else
    -- on cpu, use the existing routines to do the actual clear
    return quote
      [ W._main_wrap:Clear(storeptr, tbltype) ]
      -- and then invalidate the GPU data
      -- But make sure to also validate the CPU data fields...
      storeptr._gpu_data.store_valid.[tblvalidname]
                                    ._size_valid = Valid_CPU_Only
      escape for iField,Field in ipairs(fields) do
        local fvname          = W._cv_cache[Field].name
        emit quote
          storeptr._gpu_data.store_valid.[tblvalidname]
                                        .[fvname] = Valid_CPU_Only
        end
      end end
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
  local fields        = Table:_INTERNAL_fields()
  -- invalidate the GPU data
  -- and validate the CPU data fields...
  local stmts = newlist()
  stmts:insert(quote 
    storeptr._gpu_data.store_valid.[tblvalidname]
                                  ._size_valid = Valid_CPU_Only
  end)
  for iField,Field in ipairs(fields) do
    local fvname      = self._cv_cache[Field].name
    stmts:insert(quote
      storeptr._gpu_data.store_valid.[tblvalidname]
                                    .[fvname] = Valid_CPU_Only
    end)
  end
  return quote [stmts] end
end

-- do not need...
--function GWrapper:CPU_Global(storeptr, tbltype)
--  local W             = self
--  local Table, main_tblname, sub_tblname, tblvalidname
--                      = unpack_prepare_tbl(W, tbltype)
--  local metatbl       = (`storeptr._gpu_data.cpu_meta.[sub_tblname])
--  return quote [metatbl]:size_from_gpu(storeptr) end
--end


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


function GWrapper:ScanAB(name, storeptr, gpu_tblptr, gpu_globptr,
                         tbl0type, row0sym, tbl1type, row1sym,
                         args, bodycode
)
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
    var SZ0_hi    = (SIZE0+7)/8
    var SZ1_hi    = (SIZE1+7)/8
    var N_launch  = SZ0_hi * SZ1_hi * 64
    -- extract the tblptr and globptr
    var tblptr    = storeptr._gpu_data.gpu_meta
    var globptr   = storeptr._gpu_data.gpu_globals
    -- launch the GPU kernel
    gpu_kernel(N_launch, tblptr, globptr, [args])
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
    C.printf("WARNING TODO: need to tighten symmetric launch...\n")
    -- figure out how many threads to launch
    var SIZE      = storeptr.[main_tblname]:size()
    var SZ_hi     = (SIZE+7)/8
    var N_launch  = SZ_hi * SZ_hi * 64
    -- extract the tblptr and globptr
    var tblptr    = storeptr._gpu_data.gpu_meta
    var globptr   = storeptr._gpu_data.gpu_globals
    -- launch the GPU kernel
    gpu_kernel(N_launch, tblptr, globptr, [args])
  end
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

  local stmts           = newlist()
  -- write values into the fields at that row
  for iField,Field in ipairs(Table:fields()) do
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
    -- allocate a row for the insertion
    var MAX_SIZE        = gpuptr.[sub_tblname]._alloc_size
    var tid             = GPU.global_tid()
    var row             = GPU.reserve_idx(tid, &(gpuptr.[sub_tblname]._size))
    -- Overflow Error Condition
    if row >= MAX_SIZE then GPU.trap() end
    -- and write values into the fields at that row
    [ GW:WriteRow(gpuptr, tbltype, row, vals) ]
  end --in row end -- might as well return the new row value
end







