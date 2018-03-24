
local Exports = {}
package.loaded["gong.src.genapi"] = Exports


local T             = require 'gong.src.types'
local is_type       = T.is_type

local Effects       = require 'gong.src.effectcheck'
local E             = Effects.Effects

local Util          = require 'gong.src.util'
local INTERNAL_ERR  = Util.INTERNAL_ERR

local DataStore     = require 'gong.src.datastore'
GenerateDataWrapper = DataStore.GenerateDataWrapper


local newlist       = terralib.newlist


-------------------------------------------------------------------------------
-- Generate Basic Library
-------------------------------------------------------------------------------

local function table_join_closure(tables, joins)
  local TABLES          = {}
  local JOINS           = {}
  for _,tbl in ipairs(tables) do TABLES[tbl] = true end
  for _,j   in ipairs(joins)  do JOINS[j]    = true end

  -- form closure of all tables refered to in joins
  for _,j   in ipairs(joins) do
    -- pull tables out of scan/emit effects
    for _,e in ipairs(j:_INTERNAL_geteffects()) do
      if E.Scan.check(e) then
        TABLES[e.src:table()]   = true
      elseif E.Emit.check(e) then
        TABLES[e.dst:table()]   = true
      elseif E.Merge.check(e) then
        TABLES[e.dst:table()]   = true
      end
    end
  end

  -- form closure of all tables referring to each other
  local init_tables     = {}
  for t,_ in pairs(TABLES) do init_tables[t] = true end
  local add_table, add_table_type
  function add_table_type(typ)
    if typ:is_row() then            add_table(typ:table())
    elseif typ:is_record() then
      for _,fp in ipairs(typ.fields) do
        if fp.type:has_rows() then  add_table_type(fp.type) end
      end
    elseif typ:is_tensor() then     add_table_type(typ:basetype())
    else INTERNAL_ERR('sanity') end
  end
  function add_table(tbl)
    TABLES[tbl]         = tbl
    for _,f in ipairs(tbl:_INTERNAL_fields()) do
      if f:type():has_rows() then
        add_table_type(f:type())
      end
    end
  end
  for t,_ in pairs(init_tables) do add_table(t) end

  -- read out those lists
  local tables, joinfuncs = newlist(), newlist()
  for t,_ in pairs(TABLES) do tables:insert(t) end
  for j,_ in pairs(JOINS)  do joinfuncs:insert(j) end

  return tables, joinfuncs
end

local compile_lib_err_msg = [[
Must supply named arguments to CompileLibrary{}:
  prefix          = 'string'  (optional)
  tables          = { Table } (list of data tables)
  joins           = { Join  } (list of joins)
  c_obj_file      = 'string'  (filename for library object code)
  c_header_file   = 'string'  (filename for library header file)
  cpp_header_file = 'string'  (filename for library header file)
  terra_out       = bool,     (use instead of filenames to emit Terra API)
]]
function Exports.CompileLibrary(args)
  if type(args) ~= 'table' or
     (args.prefix and type(args.prefix) ~= 'string') or
     not terralib.israwlist(args.tables) or
     not terralib.israwlist(args.joins) or
     not ((
            type(args.c_obj_file) == 'string' and
            type(args.c_header_file) == 'string'
          ) or (
            args.terra_out == true
          ) or (
            type(args.c_obj_file) == 'string' and
            type(args.cpp_header_file) == 'string'
         ))
  then
    error(compile_lib_err_msg, 2)
  end

  if (args.terra_out and args.c_obj_file) or
     (args.c_header_file and args.cpp_header_file)
  then
    error("cannot compile for more than one language simultaneously", 2)
  end
  if #args.joins <= 0 then
    error('expected compilation to specify at least one join to compile', 2)
  end

  -- form a closure to make sure we generate everything we need
  local tables, joins   = table_join_closure(args.tables, args.joins)

  -- go through the joins and modify table features
  -- to ensure the needed functionality
  for _,j in ipairs(joins) do
    for _,eff in ipairs(j:_INTERNAL_geteffects()) do
      if E.Merge.check(eff) then
        eff.dst:table():_INTERNAL_ActivateMergeIndex()
      end
    end
  end

  -- Extract Indices
  local indices         = newlist()
  for _,tbl in ipairs(tables) do
    indices:insertall( tbl:_INTERNAL_GetIndices() )
  end

  -- language mode argument parsing
  local langmode        = (args.terra_out and 'terra') or
                          (args.c_header_file and 'c') or
                          (args.cpp_header_file and 'cpp') or
                          nil
  local prefix          = args.prefix or ''
  if langmode=='cpp' then
    prefix              = 'C_'..prefix
  end

  -- generate wrapper
  local W               = GenerateDataWrapper(prefix, tables, indices)

  -- Assemble all the structs and funcs to expose
  local FUNCS, STRUCTS, HIERARCHY =
                          W:GenExternCAPI(prefix, joins)

  if langmode=='terra' then
    local API           = Exports.GenTerraAPI(HIERARCHY)
    return API
  elseif langmode=='cpp' then
    -- put Terra structs and funcs into exportable format
    local HEADER_STR, FUNC_TABLE  = Exports.GenCppAPI(prefix,
                                                      STRUCTS,
                                                      FUNCS,
                                                      HIERARCHY)

    -- write out the header file
    local headerF   = io.open(args.cpp_header_file, "w")
    headerF:write(HEADER_STR)
    headerF:close()

    -- compile out the object file
    terralib.saveobj(args.c_obj_file,
                     "object",
                     FUNC_TABLE)
  else -- C output
    -- put Terra structs and funcs into exportable format
    local HEADER_STR, FUNC_TABLE  = Exports.GenCAPI(prefix,
                                                    STRUCTS,
                                                    FUNCS)

    -- write out the header file
    local headerF   = io.open(args.c_header_file, "w")
    headerF:write(HEADER_STR)
    headerF:close()

    -- compile out the object file
    terralib.saveobj(args.c_obj_file,
                     "object",
                     FUNC_TABLE)
  end
end


-------------------------------------------------------------------------------
-- Terra API generation
-------------------------------------------------------------------------------

function Exports.GenTerraAPI(hierarchy)
  local ROOT              = hierarchy
  local API               = {}

  local Store             = ROOT.Store
  API.Store               = Store
  API.NewStore            = ROOT.NewStore
  terra Store:destroy()
    ROOT.DestroyStore( @self )
  end
  terra Store:geterror() : rawstring
    return ROOT.GetError( @self )
  end

  for jname,j in pairs(ROOT.joins) do
    local args            = j:gettype().parameters:sublist(2)
                             :map(function(t) return symbol(t) end)
    Store.methods[jname]  = terra( self : &Store, [args] )
      j(@self, [args])
    end
    Store.methods[jname]:setname(jname)
  end

  for tname,TBL in pairs(ROOT.tables) do
    local TWrap           = terralib.types.newstruct(tname)
    TWrap.entries:insert{ field='store', type=Store }
    Store.methods[tname]  = terra( self : &Store ) : TWrap
      return [TWrap]({ @self })
    end
    Store.methods[tname]:setname(tname)

    if TBL.GetSize then
      terra TWrap:getsize() return TBL.GetSize(self.store) end
    else
      terra TWrap:get_n_rows() return TBL.GetNRows(self.store) end
      terra TWrap:get_n_alloc() return TBL.GetNAlloc(self.store) end
    end
    local loadarg         = symbol(TBL.BeginLoad:gettype().parameters[2])
    terra TWrap:beginload( [loadarg] )
      TBL.BeginLoad(self.store, loadarg)
    end
    terra TWrap:endload() TBL.EndLoad(self.store) end
    local loadrowargs     = TBL.LoadRow:gettype().parameters:sublist(2)
                                       :map(function(t) return symbol(t) end)
    terra TWrap:loadrow( [loadrowargs] )
      TBL.LoadRow(self.store, [loadrowargs])
    end

    for fname,FLD in pairs(TBL.fields) do
      local FWrap         = terralib.types.newstruct(tname..'_'..fname)
      FWrap.entries:insert{ field='store', type=Store }
      TWrap.methods[fname]  = terra( self : &TWrap ) : FWrap
        return [FWrap]({ self.store })
      end
      TWrap.methods[fname]:setname(fname)

      local rowtype       = FLD.Read:gettype().parameters[2]
      local ftype         = FLD.Read:gettype().returntype
      terra FWrap:read( r : rowtype ) : ftype
        return FLD.Read(self.store, r)
      end
      terra FWrap:read_lock() : &ftype
        return FLD.ReadLock(self.store)
      end
      terra FWrap:read_unlock()
        FLD.ReadUnlock(self.store)
      end

      if fname == 'is_live' then
        -- nothing for now
      else
        terra FWrap:load( ptr : &ftype, stride : uint32 )
          FLD.Load(self.store, ptr, stride)
        end
        terra FWrap:write( r : rowtype, v : ftype )
          FLD.Write(self.store, r, v)
        end
        terra FWrap:readwrite_lock() : &ftype
          return FLD.ReadWriteLock(self.store)
        end
        terra FWrap:readwrite_unlock()
          FLD.ReadWriteUnlock(self.store)
        end
      end
    end
  end

  return API
end


-------------------------------------------------------------------------------
-- C API generation
-------------------------------------------------------------------------------

local function C_Common_API(prefix, structs, funcs)
  local NAMES             = {}
  local function addname(name)
    if NAMES[name] then error("name collision: "..name) end
    NAMES[name]             = true
  end
  local STRUCT_SIGS       = newlist()
  local EXTRA_STRUCTS     = newlist()
  local FUNC_SIGS         = newlist()
  local FUNC_TABLE        = {}

  local TYPE_TRANSLATE = {
    [bool]      = 'bool',
    [float]     = 'float',
    [double]    = 'double',
    [int8]      = 'int8_t',
    [int16]     = 'int16_t',
    [int32]     = 'int32_t',
    [int64]     = 'int64_t',
    [uint8]     = 'uint8_t',
    [uint16]    = 'uint16_t',
    [uint32]    = 'uint32_t',
    [uint64]    = 'uint64_t',
  }
  local struct_sig
  local function translate_type(typ)
    if TYPE_TRANSLATE[typ] then return TYPE_TRANSLATE[typ] end

    local translation       = nil

    if typ:isunit() then
      TYPE_TRANSLATE[typ]   = 'void'
      return 'void'
    elseif typ:isstruct() then
      local name            = tostring(typ)
      addname(name)
      TYPE_TRANSLATE[typ]   = name
      local sig             = struct_sig(typ)
      EXTRA_STRUCTS:insert(   sig )
      return name
    elseif typ:ispointer() then
      local subtyp          = translate_type(typ.type)
      TYPE_TRANSLATE[typ]   = subtyp..'*'
      return subtyp..'*'
    elseif typ:isarray()  then
      local subtyp          = translate_type(typ.type)
      TYPE_TRANSLATE[typ]   = subtyp..'['..typ.N..']'
      return TYPE_TRANSLATE[typ]
    else INTERNAL_ERR('unexpected type to translate: '..tostring(typ)) end
  end
  function struct_sig(s)
    local name              = tostring(s)
    local strs              = newlist{ 'typedef struct {'}
    for _,e in ipairs(s.entries) do
      local fname, typ      = e.field, e.type
      if fname == nil then
        fname, typ          = e[1], e[2]
      end
      local typstr          = translate_type(typ)
      local arr_suffix      = ''
      if typ:isarray() then
        local base, count   = typstr:match('([%w_]+)%[(%d+)%]')
        typstr, arr_suffix  = base, '['..count..']'
      end
      strs:insert('  '..typstr..' '..fname..arr_suffix..';')
    end
    strs:insert( '} '..name..';' )
    return strs:concat('\n')
  end

  -- stub in all the explicit structs first
  for _,s in ipairs(structs) do
    local name              = tostring(s)
    addname(name)
    TYPE_TRANSLATE[s]       = name
  end

  -- translate the explicit structs
  for _,s in ipairs(structs) do
    local sig               = struct_sig(s)
    STRUCT_SIGS:insert(sig)
  end

  -- translate the functions
  for _,f in ipairs(funcs) do
    if type(f) == 'string' then
      FUNC_SIGS:insert(f)
    else
      local name            = f:getname()
      --addname(name)

      local rettype         = translate_type(f:gettype().returntype)
      local args            = newlist()
      for i,at in ipairs(f:gettype().parameters) do
        args:insert(          translate_type(at) )
      end

      addname(prefix..name)
      local sig = rettype..' '..prefix..name..'( '..args:concat(', ')..' );'

      FUNC_SIGS:insert(sig)
      FUNC_TABLE[prefix..name] = f
    end
  end

  local HEADER_BODY       = newlist()

  -- Structs, in two groups, then function signatures
  HEADER_BODY:insertall(STRUCT_SIGS)
  HEADER_BODY:insert('')
  HEADER_BODY:insertall(EXTRA_STRUCTS)
  HEADER_BODY:insert('')
  HEADER_BODY:insertall(FUNC_SIGS)

  return HEADER_BODY:concat('\n'), FUNC_TABLE, translate_type
end


function Exports.GenCAPI(prefix, structs, funcs)
  prefix                          = (prefix and prefix..'_') or ''
  local HEADER_BODY, FUNC_TABLE   = C_Common_API(prefix, structs, funcs)

  -- Create Header File String
  local HEADER                    = newlist()
  HEADER:insertall {
    '/*',
    '  This library was auto-generated by Gong.',
    '*/',
    '',
    '#ifndef _'..prefix..'GONG_H_',
    '#define _'..prefix..'GONG_H_',
    '',
    '#include <stdint.h>',
    '#define bool uint8_t',
    '',
  }

  HEADER:insert(HEADER_BODY)

  -- close the file
  HEADER:insertall{
    '#endif /* _'..prefix..'GONG_H_ */',
    '',
  }
  local HEADER_FILE = HEADER:concat('\n')

  return HEADER_FILE, FUNC_TABLE
end


-------------------------------------------------------------------------------
-- C++ API generation
-------------------------------------------------------------------------------

--[[

Here's a snippet of a C header file.

    We're gonna want to get that
    /* New and Destroy Store; Error Handling */
    C_Store C_NewStore(  );
    void C_DestroyStore( C_Store );
    int8_t* C_GetError( C_Store );

    /* Exported Joins */
    void C_aboff( C_Store );

    /*
    Table A {
      id : int32
      val : __s_double__t_int32__
    }
    */
    uint32_t C_GetSize_A( C_Store );
    void C_BeginLoad_A( C_Store, uint32_t );
    void C_EndLoad_A( C_Store );
    void C_LoadRow_A( C_Store, int32_t, __s_double__t_int32__ );
    void C_Load_A_id( C_Store, int32_t*, uint32_t );
    int32_t C_Read_A_id( C_Store, uint32_t );
    void C_Write_A_id( C_Store, uint32_t, int32_t );
    int32_t* C_ReadWriteLock_A_id( C_Store );
    void C_ReadWriteUnLock_A_id( C_Store );

The CPP header file will need to include that as a block, but then further
generate a CPP Class/Classes that make access more convenient...

    class Store {
    private:
      C_Store store;
    public:
      static new() { Store s; s.store = C_NewStore(); return s; }
      void destroy() { C_DestroyStore(store); }
      char* geterror() { return C_GetError(store); };

      void aboff() { C_aboff(store); }
    
      class _A {
      private:
        C_Store store;
      public:
        _A(C_Store store_) : store(store_) {}

        uint32_t size() { return C_GetSize_A(store); }
        void beginload(uint32_t a0) { C_BeginLoad_A(store, a0); }
        void endload() { C_EndLoad_A(store); }
        void loadrow( int32_t a0, __s_double__t_int32__ a1 ) { C_LoadRow_A(store, a0, a1); }

        class _id {
        private:
          C_Store store;
        public:
          _id(C_Store store_) : store(store_) {}

          void load( int32_t* a0, uint32_t a1 ) { C_Load_A_id(store, a0, a1); }
          int32_t read( uint32_t a0 ) { return C_Read_A_id(store, a0); }
          void write( uint32_t a0, int32_t a1 ) { C_Read_A_id(store, a0, a1); }
          int32_t* readwrite_lock() { return C_ReadWriteLock_A_id(store); }
          void readwrite_unlock() { C_ReadWriteUnLock_A_id(store); }
        };
        _id id() { return _id(store); }

        ...
      };
      _A A() { return _A(store); }
      
      ...
    };

--]]

function Exports.GenCppAPI(prefix, structs, funcs, hierarchy)
  prefix                  = (prefix and prefix..'_') or ''
  local HEADER_BODY, FUNC_TABLE, translate_type
                          = C_Common_API(prefix, structs, funcs)
  local ROOT              = hierarchy

  local CPP               = newlist()

  local function tmethod(tab, shortname, tfunc)
    local sigarg, callarg = newlist(), newlist{'store'}
    for i,t in ipairs(tfunc:gettype().parameters:sublist(2)) do
      sigarg:insert( translate_type(t)..' a'..(i-1) )
      callarg:insert( 'a'..(i-1) )
    end
    local rettype         = translate_type(tfunc:gettype().returntype)
    local retadd          = (rettype=='void' and '') or 'return '
    local str = tab..rettype..' '..shortname..
                '('..sigarg:concat(', ')..') { '..retadd..
                prefix..tfunc:getname()..'('..callarg:concat(', ')..'); }'
    return str
  end

  CPP:insertall {
   'class Store {',
   'private:',
   '  '..prefix..'Store store;',
   'public:',
  }
  CPP:insertall {
   '  static Store NewStore() { ',
   '    Store s; s.store = '..prefix..'NewStore(); return s;',
   '  }',
   '  void destroy() { '..prefix..'DestroyStore(store); }',
   '  char* geterror() { return (char*)('..prefix..'GetError(store)); };',
   '  ',
  }
  for jname,j in pairs(ROOT.joins) do
    CPP:insert( tmethod('  ', jname, j) )
  end
  CPP:insert('  ')

  for tname,TBL in pairs(ROOT.tables) do
    CPP:insertall {
   '  class _'..tname..' {',
   '  private:',
   '    '..prefix..'Store store;',
   '  public:',
   '    _'..tname..'('..prefix..'Store store_) : store(store_) {}',
   '    ',
    }
    if TBL.GetSize then
      CPP:insert( tmethod('    ', 'size', TBL.GetSize) )
    else
      CPP:insert( tmethod('    ', 'n_rows', TBL.GetNRows) )
      CPP:insert( tmethod('    ', 'n_alloc', TBL.GetNAlloc) )
    end
    CPP:insertall {
      tmethod('    ', 'beginload', TBL.BeginLoad),
      tmethod('    ', 'endload', TBL.EndLoad),
      tmethod('    ', 'loadrow', TBL.LoadRow),
   '    ',
    }

    for fname,FLD in pairs(TBL.fields) do
      CPP:insertall {
   '    class _'..fname..' {',
   '    private:',
   '      '..prefix..'Store store;',
   '    public:',
   '      _'..fname..'('..prefix..'Store store_) : store(store_) {}',
   '      ',
        tmethod('      ','read',FLD.Read),
        tmethod('      ','read_lock',FLD.ReadLock),
        tmethod('      ','read_unlock',FLD.ReadUnlock),
      }
      if fname == 'is_live' then
        -- nada
      else
        CPP:insertall {
          tmethod('      ','load',FLD.Load),
          tmethod('      ','write',FLD.Write),
          tmethod('      ','readwrite_lock',FLD.ReadWriteLock),
          tmethod('      ','readwrite_unlock',FLD.ReadWriteUnlock),
        }
      end
      CPP:insertall {
   '    };',
   '    _'..fname..' '..fname..'() { return _'..fname..'(store); }',
   '    ',
      }
    end

    CPP:insertall {
   '  };',
   '  _'..tname..' '..tname..'() { return _'..tname..'(store); }',
   '  ',
    }
  end
  CPP:insertall {
   '};',
  }


  -- Create Header File String
  local HEADER                    = newlist()
  HEADER:insertall {
    '/*',
    '  This library was auto-generated by Gong.',
    '*/',
    '',
    '#ifndef _'..prefix..'GONG_H_',
    '#define _'..prefix..'GONG_H_',
    '',
    '#include <cstdint>',
    --'#define bool uint8_t',
    '',
  }

  HEADER:insertall{
    'extern "C" {',
    HEADER_BODY,
    '}',
    '',
  }
  HEADER:insertall(CPP)

  -- close the file
  HEADER:insertall{
    '',
    '#endif /* _'..prefix..'GONG_H_ */',
    '',
  }
  local HEADER_FILE = HEADER:concat('\n')

  return HEADER_FILE, FUNC_TABLE
end

