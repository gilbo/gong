--[[
  schemata - Basic Building Block of Data Sets

  See LICENSE
--]]
import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.schemata"] = Exports


local Util        = require 'gong.src.util'
local T           = require 'gong.src.types'
local is_id_str   = Util.is_id_str
local is_pos_int  = Util.is_pos_int

local newlist = terralib.newlist

local universalKeyType    = uint32
local universal_MAX_SIZE  = math.pow(2,32)-3

-------------------------------------------------------------------------------
-- DataTable Object
-------------------------------------------------------------------------------

local join_policy_list = {
  ['none']      = true,
--  ['rebuild']   = true,
--  ['transfer']  = true,
}

local DataTable       = {}
DataTable.__index     = DataTable
function DataTable:__newindex(key, val)
  error("Cannot assign members to Data Table object "..
        "(did you mean to call tbl:NewField()?)", 2) end

local function NewTable(name, options)
  if not is_id_str(name) then
    error('expected valid name string as first argument', 2) end
  options = options or {}

  local join_policy = options.join_policy or 'none'
  if not join_policy_list[join_policy] then
    error('unrecognized join policy: '..join_policy, 2)
  end

  return setmetatable({
    _name           = name,
    _join_policy    = join_policy,
    _primary_key    = newlist(),
    _user_fields    = newlist(),
    _fields         = newlist(),
    _fields_by_name = {},
    _key_rep        = universalKeyType,
    _indices        = newlist(),
    _reffed_by      = {},
    _ref_list       = nil,
    _is_sealed      = false,
    _gong_record    = false,
    _mergeable      = false,
  }, DataTable)
end
Exports.NewTable    = NewTable

local function is_table(obj)
  return getmetatable(obj) == DataTable
end
Exports.is_table    = is_table


-------------------------------------------------------------------------------
-- Field Object
-------------------------------------------------------------------------------

local DataField       = {}
DataField.__index     = DataField
function DataField:__newindex(key, val)
  error("Cannot assign members to Data Field object", 2) end

local function mark_ref(src, typ)
  if typ:is_row() then
    typ:table()._reffed_by[src] = true
  elseif typ:is_tensor() then
    mark_ref(src, typ:basetype())
  elseif typ:is_record() then
    for _,f in ipairs(typ.fields) do
      mark_ref(src, f.type)
    end
  end
end

local function NewFieldInternal(params)
  assert(type(params) == 'table')

  local f = setmetatable({
    _name     = assert(params.name),
    _type     = assert(params.type),
    _parent   = assert(params.parent),
    _id       = assert(params.id),
  }, DataField)

  if f:type():has_rows() then mark_ref(f:table(), f:type()) end

  return f
end

local function is_field(obj) return getmetatable(obj) == DataField end
Exports.is_field    = is_field


-------------------------------------------------------------------------------
-- Index Object
-------------------------------------------------------------------------------

local DataIndex   = {}
DataIndex.__index = DataIndex
function DataIndex:__newindex(key, val)
  error("Cannot assign members to Data Index object", 2) end

local function NewIndexInternal(params)
  assert(type(params) == 'table')
  assert(is_table(params.dst))
  assert(terralib.islist(params.src))
  for _,f in ipairs(params.src) do
    assert(is_field(f) and
           f:type():is_row() and
           f:table() == params.dst) end
  local idx = setmetatable({
    _name         = assert(params.name),
    _dst_tbl      = params.dst,
    _src_fields   = params.src:copy(),
  }, DataIndex)
  return idx
end

local function is_index(obj) return getmetatable(obj) == DataIndex end
Exports.is_index = is_index


-------------------------------------------------------------------------------
-- Methods
-------------------------------------------------------------------------------

function DataTable:complete()
  if self._is_sealed then return end
  self._is_sealed = true
  -- generate a backing record type now
  local fs = newlist()
  -- note that we are not going to include any hidden fields
  for i,f in ipairs(self._user_fields) do
    fs:insert { f:name(), f:type() }
  end
  self._gong_record = T.record(fs)
end
function DataTable:is_complete()  return self._is_sealed      end

function DataTable:SetPrimaryKey(...)
  local args  = newlist{...}
  if #args == 0 then
    error("SetPrimaryKey() expects 1 or more arguments", 2)
  elseif #self._primary_key > 0 then
    error("Primary Key already set for table '"..self:name().."'", 2)
  elseif #args ~= 2 then
    error("SetPrimaryKey() currently expects exactly 2 arguments", 2)
  end
  -- check argument format, and convert to fields
  local fs    = newlist()
  for i,a in ipairs(args) do
    if is_field(a) then
      if a:table() ~= self then
        error("bad SetPrimaryKey() argument #"..i..
              ": expects fields of table '"..self:name().."'", 2)
      end
      fs:insert(a)
    elseif type(a) == 'string' then
      local f = self._fields_by_name[a]
      if not f then
        error("bad SetPrimaryKey() argument #"..i..
              ": could not find field named '"..a.."'", 2)
      end
      fs:insert(f)
    else
      error("bad SetPrimaryKey() argument #"..i..
            ": expects strings or fields as arguments", 2)
    end
  end
  -- check that keys are rows
  for i,f in ipairs(fs) do
    if not f:type():is_row() then
      error("bad SetPrimaryKey() argument #"..i..
            ": expect row-type fields as primary keys", 2)
    end
  end

  self._primary_key:insertall(fs)
  return self
end

function DataTable:name()         return self._name               end
function DataTable:join_policy()  return self._join_policy        end
function DataTable:primary_key()  return self._primary_key:copy() end
function DataTable:fields(id)
  self:complete()
  if     id == nil            then return self._user_fields:copy()
  elseif is_pos_int(id)       then return self._user_fields[id]
                              else return self._fields_by_name[id] end
end
function DataTable:__index(key)
  local lookup = rawget(DataTable, key)
  if  lookup ~= nil then return lookup
  elseif key ~= nil then return self:fields(key) end
end
function DataTable:_INTERNAL_terra_key_type()
  return self._key_rep
end
function DataTable:_INTERNAL_terra_size_type()
  return self._key_rep
end
function DataTable:_INTERNAL_terra_MAX_SIZE()
  return universal_MAX_SIZE
end
function DataTable:record_type()
  self:complete()
  return self._gong_record
end
function DataTable:reflist()
  if not self._ref_list then
    rawset(self, '_ref_list', newlist())
    for r,_ in pairs(self._reffed_by) do self._ref_list:insert(r) end
  end
  return self._ref_list
end
function DataTable:_INTERNAL_fields()
  return self._fields:copy()
end

function DataTable:_INTERNAL_ActivateMergeIndex()
  self:complete()
  assert(#self._primary_key > 0,
         "INTERNAL: expected table to have primary key")
  if not self._mergeable then
    assert(#self:reflist() == 0,
           "INTERNAL: cannot merge into a table referenced by other tables")

    self._mergeable   = true
    assert(self._fields_by_name['is_live'] == nil,
           "INTERAL: expect field-name is_live to be unused")
    local live_field  = NewFieldInternal {
      name      = 'is_live',
      type      = T.bool,
      parent    = self,
      id        = #self._fields + 1
    }
    rawset(self, '_is_live', live_field)
    self._fields:insert(live_field)

    -- add an index
    assert(#self._primary_key == 2, 'INTERNAL: expect 2 primary-key fields')
    local f0, f1    = unpack(self._primary_key)
    local name      = 'by_'..f0:name()..'_'..f1:name()
    local idx       = NewIndexInternal {
      name    = name,
      dst     = self,
      src     = self._primary_key,
    }
    self._indices:insert(idx)
  end
end
function DataTable:_INTERNAL_HasMergeIndex()
  return self._mergeable
end
function DataTable:_INTERNAL_GetIndices()
  return self._indices:copy()
end


function DataTable:NewField(name, typ)
  if self:is_complete() then
    error('cannot add fields to a completed table', 2)  end
  if not is_id_str(name) then
    error('expected valid name string as arg 1', 2)     end
  if self._fields_by_name[name] then
    error("data table '"..self:name().."' already has a field named "..
          "'"..name.."'", 2)                            end
  if name == 'is_live' then
    error("The field name 'is_live' is reserved for internal use", 2) end

  -- type promotion before testing
  if is_table(typ) then typ = T.row(typ) end

  if not T.is_type(typ) then
    error('expected gong type as arg 2', 2)             end


  local f = NewFieldInternal {
    name    = name,
    type    = typ,
    parent  = self,
    id      = #self._fields + 1,
  }
  self._fields:insert(f)
  self._user_fields:insert(f)
  self._fields_by_name[name] = f

  return self -- this table, not this field
end


function DataField:name()         return self._name           end
function DataField:fullname()
  return self._parent:name()..'.'..self._name                 end
function DataField:type()         return self._type           end
function DataField:id()           return self._id             end
function DataField:table()        return self._parent         end


function DataIndex:name()         return self._name               end
function DataIndex:fullname()
  return self._dst_tbl:name()..'.'..self._name                    end
function DataIndex:dst()          return self._dst_tbl            end
function DataIndex:srcs()         return self._src_fields:copy()  end










