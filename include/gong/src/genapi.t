
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
    for _,f in ipairs(tbl:fields()) do
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
  prefix        = 'string'  (optional)
  tables        = { Table } (list of data tables)
  joins         = { Join  } (list of joins)
  c_obj_file    = 'string'  (filename for library object code)
  c_header_file = 'string'  (filename for library header file)
]]
function Exports.CompileLibrary(args)
  if type(args) ~= 'table' or
     (args.prefix and type(args.prefix) ~= 'string') or
     not terralib.israwlist(args.tables) or
     not terralib.israwlist(args.joins) or
     type(args.c_obj_file) ~= 'string' or
     type(args.c_header_file) ~= 'string'
  then
    error(compile_lib_err_msg, 2)
  end

  if #args.joins <= 0 then
    error('expected compilation to specify at least one join to compile', 2)
  end

  -- form a closure to make sure we generate everything we need
  local tables, joins   = table_join_closure(args.tables, args.joins)

  -- generate wrapper
  local W               = GenerateDataWrapper(args.prefix, tables)

  -- Assemble all the structs and funcs to expose
  local FUNCS, STRUCTS  = W:GenExternCAPI(args.prefix, joins)
  ---- Add the Join functions to the API next
  --for _,jf in ipairs(joins) do
  --  FUNCS:insert( W:get_terra_function(jf, {exposed=true}) )
  --end

  -- put Terra structs and funcs into exportable format
  local HEADER_STR, FUNC_TABLE  = Exports.GenCAPI(args.prefix,
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


-------------------------------------------------------------------------------
-- C API generation
-------------------------------------------------------------------------------

function Exports.GenCAPI(prefix, structs, funcs)
  prefix                  = (prefix and prefix..'_') or ''

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
      strs:insert('  '..typstr..' '..fname..';')
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
      addname(name)

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

  -- Create Header File String
  local HEADER            = newlist()
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

  -- Structs, in two groups, then function signatures
  HEADER:insertall(STRUCT_SIGS)
  HEADER:insert('')
  HEADER:insertall(EXTRA_STRUCTS)
  HEADER:insert('')
  HEADER:insertall(FUNC_SIGS)
  HEADER:insert('')

  -- close the file
  HEADER:insertall{
    '#endif /* _'..prefix..'GONG_H_ */',
    '',
  }
  local HEADER_FILE = HEADER:concat('\n')

  return HEADER_FILE, FUNC_TABLE
end


