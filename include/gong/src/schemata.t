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

local universalKeyType = uint32

-------------------------------------------------------------------------------
-- DataTable Object
-------------------------------------------------------------------------------

local join_policy_list = {
  ['none']      = true,
  ['rebuild']   = true,
  ['transfer']  = true,
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
    _fields         = newlist(),
    _fields_by_name = {},
    _key_rep        = universalKeyType,
    _is_sealed      = false,
    _gong_record    = false,
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

local function NewFieldInternal(params)
  assert(type(params) == 'table')

  local f = setmetatable({
    _name     = assert(params.name),
    _type     = assert(params.type),
    _parent   = assert(params.parent),
    _id       = assert(params.id),
  }, DataField)
  return f
end

local function is_field(obj) return getmetatable(obj) == DataField end


-------------------------------------------------------------------------------
-- Methods
-------------------------------------------------------------------------------

function DataTable:complete()
  if self._is_sealed then return end
  self._is_sealed = true
  -- generate a backing record type now
  local fs = newlist()
  -- note that we are not going to include any hidden fields
  for i,f in ipairs(self._fields) do
    fs:insert { f:name(), f:type() }
  end
  self._gong_record = T.record(fs)
end
function DataTable:is_complete()  return self._is_sealed      end

function DataTable:name()         return self._name           end
function DataTable:join_policy()  return self._join_policy    end
function DataTable:fields(id)
  self:complete()
  if     id == nil            then return self._fields:copy()
  elseif is_pos_int(id)       then return self._fields[id]
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
function DataTable:record_type()
  self:complete()
  return self._gong_record
end

function DataTable:NewField(name, typ)
  if self:is_complete() then
    error('cannot add fields to a completed table', 2)  end
  if not is_id_str(name) then
    error('expected valid name string as arg 1', 2)     end
  if self._fields_by_name[name] then
    error("data table '"..self:name().."' already has a field named "..
          "'"..name.."'", 2)                            end

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
  self._fields_by_name[name] = f

  return self -- this table, not this field
end

function DataField:name()         return self._name           end
function DataField:fullname()
  return self._parent:name()..'.'..self._name                 end
function DataField:type()         return self._type           end
function DataField:id()           return self._id             end
function DataField:table()        return self._parent         end











