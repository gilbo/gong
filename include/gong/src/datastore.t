--import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.datastore"] = Exports

-------------------------------------------------------------------------------

--local Typechecker = require 'gong.src.typechecker'
local T           = require 'gong.src.types'
local Util        = require 'gong.src.util'
--local AST         = Typechecker.AST
local is_type     = T.is_type
--local SrcInfo     = Util.SrcInfo
--local NewSymbol   = Util.NewSymbol
--local is_symbol   = Util.is_symbol
local is_id_str   = Util.is_id_str
--local is_int      = Util.is_int
local INTERNAL_ERR  = Util.INTERNAL_ERR

local Schemata    = require 'gong.src.schemata'
--local Macro       = require 'gong.src.macro'
--local Global      = require 'gong.src.global'
--local Functions   = require 'gong.src.functions'
local is_table    = Schemata.is_table
local is_field    = Schemata.is_field
--local is_quote    = Macro.is_quote
--local is_constant = Global.is_constant
--local is_function = Functions.is_function
--local is_builtin  = Functions.is_builtin

local CodeGen     = require 'gong.src.codegen'

local C           = require 'gong.src.c'
local assert      = C.assert

local newlist   = terralib.newlist

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
    <Type>    ::= <primitive>
                | `row(` <Table> `)`
                | `tensor(` <Type> `,` <num>+ `)`
                | `record{` (<name> `:` <Type>)* `}`
  
  We're going to translate these down into C-data structures according to
  the following intuition:
    Schema      |-->    CStore
    Table       |-->    CTable
    Field       |-->    Column*  (we use a naive layout right now)

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


-------------------------------------------------------------------------------
--[[                           Wrapper Structure                           ]]--
-------------------------------------------------------------------------------

local Wrapper       = {}
Wrapper.__index     = Wrapper

local WRAPPER_ROOT  = {}
local ERR_BUF_LEN   = 2048

local function GenerateWrapperStructs(prefix, W)
  -- generate structures
  local CStore              = terralib.types.newstruct('InternalStore')
  -- needlessly defensive thing
  CStore.entries:insert{ '__padding__', uint64 }
  for iTable,Table in ipairs(W._tables) do
    local TableName         = Table:name()..'_'..(iTable-1)
    local CTable            = terralib.types.newstruct(TableName)
    --local KeyT              = Table:_INTERNAL_terra_key_type()
    local SizeT             = Table:_INTERNAL_terra_size_type()
    CTable.entries:insertall {
      { '_alloc_size',  SizeT },
      { '_size',        SizeT },
    }

    local fields            = Table:fields()
    for iField,Field in ipairs(fields) do
      -- blah
      local FieldName       = Field:name()..'_'..(iField-1)
      local FPtr            = &(Field:type():terratype())
      CTable.entries:insert { '_'..FieldName, FPtr }

      W._c_cache[Field]     = { ctype = FPtr,   name = '_'..FieldName }
    end

    CTable:complete()
    CStore.entries:insert { '_'..TableName, CTable }
    W._c_cache[Table]       = { ctype = CTable, name = '_'..TableName }
    W._type_reverse[CTable] = Table
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

  -- Per-Table Methods
  for iTable,Table in ipairs(W._tables) do
    local CTable            = W._c_cache[Table].ctype
    local SizeT             = Table:_INTERNAL_terra_size_type()
    local MAXsize           = Table:_INTERNAL_terra_MAX_SIZE()

    CTable.methods.size = macro(function(self) return `self._size end)

    terra CTable:resize( newsize : SizeT )
      var this              = self
      var oldalloc          = self._alloc_size
      var oldsize           = self._size
      if newsize > oldalloc then
        var newalloc        = maxf(oldalloc*2, newsize)
        assert(newalloc < MAXsize and newalloc > oldalloc,
               'exceeded max allowable size on resize()')
        escape for iField,Field in ipairs(Table:fields()) do
          local fname       = W._c_cache[Field].name
          local ftype       = Field:type():terratype()
          emit quote
            realloc_ptr( this.[fname], ftype, newalloc )
          end
        end end
        self._alloc_size    = newalloc
      end
      self._size            = newsize
    end
  end

  local CStore                = W._c_cache[WRAPPER_ROOT].ctype

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
      emit quote
        this.[tblname]._alloc_size  = MIN_INIT_SIZE
        this.[tblname]._size        = 0
      end

      local fields            = Table:fields()
      for iField,Field in ipairs(fields) do
        local fname           = W._c_cache[Field].name
        local ftype           = Field:type():terratype()
        emit quote
          this.[tblname].[fname] = alloc_ptr(ftype, MIN_INIT_SIZE)
        end
      end
    end end
  end
  terra CStore:destroy()
    var this        = self
    this._error_msg = nil
    escape for iTable,Table in ipairs(W._tables) do
      local tblname           = W._c_cache[Table].name
      local fields            = Table:fields()
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

local function NewWrapper(prefix, schema_tables)
  prefix = (prefix and prefix..'_') or ''

  local W = setmetatable({
    _tables         = newlist(schema_tables),
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
                                              f_obj._ast,
                                              self)
    self._func_cache[f_obj] = tfunc
  end
  return self._func_cache[f_obj]
end

-------------------------------------------------------------------------------
--[[                     Gong Internal Data Interface                      ]]--
-------------------------------------------------------------------------------

function Wrapper:Store_Struct()
  return self._c_cache[WRAPPER_ROOT].ctype
end

function Wrapper:Scan(storeptr, tbltype, rowsym, bodycode)
  local Table           = tbltype:table()
  local tblname         = self._c_cache[Table].name

  return quote
    var SIZE  = storeptr.[tblname]:size()
    for [rowsym]=0,SIZE do
      [bodycode]
    end
  end
end

function Wrapper:DoubleScan(storeptr, tbltype, row0sym, row1sym, bodycode)
  local Table           = tbltype:table()
  local tblname         = self._c_cache[Table].name

  return quote
    var SIZE  = storeptr.[tblname]:size()
    for [row0sym]=0,SIZE do
      for [row1sym]=row0sym,SIZE do
        [bodycode]
      end
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
  local Table           = tbltype:table()
  local tblname         = self._c_cache[Table].name
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

  return quote [stmts] in [read_val] end
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
  local Table           = tbltype:table()
  local tblname         = self._c_cache[Table].name

  assert(#path == 1 and type(path[1]) == 'string',
         'INTERNAL: field path expected')
  local Field           = assert(Table:fields(path[1]),
                                 'INTERNAL: field lookup failed')
  local fname           = self._c_cache[Field].name
  local ftype           = Field:type():terratype()

  return quote
    var SIZE    = storeptr.[tblname]:size()
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
  local Table           = tbltype:table()
  local tblname         = self._c_cache[Table].name

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
  local Table           = tbltype:table()
  local tblname         = self._c_cache[Table].name
  local tbl_expr        = `storeptr.[tblname]

  return quote
    -- allocate a row for the insertion
    var row             = tbl_expr:size()
    tbl_expr:resize(row+1)
    -- and write values into the fields at that row
    [ W:WriteRow(storeptr, tbltype, row, vals) ]
  in row end -- might as well return the new row value
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
    add_func_note("}")
    add_func_note("*/")

    -- Check Table Size
    add_func('GetSize_'..tname, HIERARCHY.tables[tname], 'GetSize',
    terra( hdl : ExtStore ) : SizeT
      return to_store(hdl).[tbl_cname]:size()
    end)

    -- Bulk Load
    add_func('BeginLoad_'..tname, HIERARCHY.tables[tname], 'BeginLoad',
    terra( hdl : ExtStore, size : SizeT )
      var store = to_store(hdl)
      if store._load_counter >= 0 or store._load_col_counter >= 0 then
        store:error('cannot begin load while another table is loading')
      end
      store._load_counter     = 0
      store._load_col_counter = 0
      store.[tbl_cname]:resize(size)
    end)
    add_func('EndLoad_'..tname, HIERARCHY.tables[tname], 'EndLoad',
    terra( hdl : ExtStore )
      var store         = to_store(hdl)
      var row_success   = ( store._load_counter < store.[tbl_cname]:size() )
      var col_success   = ( store._load_col_counter < [#Table:fields()] )
      if not row_success and not col_success then
        store:error(
          ['expected to see %d rows loaded or %d columns loaded,\n'..
           'but only %d rows and %d columns have been loaded so far\n'],
          store.[tbl_cname]:size(), [#Table:fields()],
          store._load_counter, store._load_col_counter)
      end
      store._load_counter = -1
      store._load_col_counter = -1
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
      if row >= store.[tbl_cname]:size() then
        store:error(['tried to load more than %d rows into table '..tname],
                    store.[tbl_cname]:size())
      else
        [ W:WriteRow(store, T.row(Table), row, vals) ]
      end
    end)

    HIERARCHY.tables[tname].fields  = {}
    local H_FIELDS                  = HIERARCHY.tables[tname].fields
    for iField,Field in ipairs(Table:fields()) do
      local fname           = Field:name()
      local f_cname         = W._c_cache[Field].name
      local ftype           = Field:type():terratype()

      H_FIELDS[fname]       = {}

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
    add_func_note("")
  end

return FUNCS, STRUCTS, HIERARCHY
end





