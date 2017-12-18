--[[
  schemata - Basic Building Block of Data Sets

  See LICENSE
--]]
import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.schemata"] = Exports


local Util        = require 'gong.src.util'
--local SrcInfo     = Util.SrcInfo
local is_id_str   = Util.is_id_str

local newlist = terralib.newlist

-------------------------------------------------------------------------------
-- DataTable Object
-------------------------------------------------------------------------------

local DataTable   = {}
DataTable.__index = DataTable

local function NewTable(name, options)
  if not is_id_str(name) then
    error('expected valid name string as first argument', 2) end
  options = options or {}

  return setmetatable({
    _name     = name,
  }, DataTable)
end
Exports.NewTable    = NewTable

local function is_table(obj)
  return getmetatable(obj) == DataTable
end
Exports.is_table    = is_table

-------------------------------------------------------------------------------

function DataTable:Name()   return self._name   end


