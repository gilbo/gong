--[[
  stdlib - exported Lua interface

  See LICENSE
--]]

local Exports = {}
package.loaded["gong.src.stdlib"] = Exports

--local Macro   = require 'gong.src.macro'
local T       = require 'gong.src.types'

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
-- Other
-------------------------------------------------------------------------------

-- assert?

