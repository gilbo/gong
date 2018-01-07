--[[
  stdlib - exported Lua interface

  See LICENSE
--]]

local Exports = {}
package.loaded["gong.src.stdlib"] = Exports

local Macro     = require 'gong.src.macro'
local Global    = require 'gong.src.global'
local T         = require 'gong.src.types'
local S         = require 'gong.src.schemata'
local Functions = require 'gong.src.functions'

local newlist = terralib.newlist

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

for _,name in ipairs({
  "int8",   "uint8",    "size8", 
  "int16",  "uint16",   "size16",
  "int32",  "uint32",   "size32",
  "int64",  "uint64",   "size64",
  --
  "bool",   "float",    "double",

  "row",
  "record",
  "tensor",
  "matrix",
  "vector",

  "is_type",
}) do
  Exports[name] = T[name]
end
-- vec and mat shorthands
for _,tchar in ipairs({ 'f', 'd', 'i', 'u', 'b' }) do
  for i=2,4 do
    local n = tostring(i)
    Exports['vec'..n..tchar] = T['vec'..n..tchar]
    Exports['mat'..n..tchar] = T['mat'..n..tchar]
    for j=2,4 do
      local m = tostring(j)
      Exports['mat'..n..'x'..m..tchar] = T['mat'..n..'x'..m..tchar]
    end
  end
end


-------------------------------------------------------------------------------
-- Data API
-------------------------------------------------------------------------------

Exports.NewTable  = S.NewTable


-------------------------------------------------------------------------------
-- Other (Assorted)
-------------------------------------------------------------------------------

-- assert?

Exports.Macro     = Macro.NewMacro
Exports.is_macro  = Macro.is_macro
Exports.is_quote  = Macro.is_quote

Exports.Constant    = Global.NewConstant
Exports.is_constant = Global.is_constant

Exports.is_function = Functions.is_function
Exports.is_builtin  = Functions.is_builtin
Exports.is_join     = Functions.is_join








