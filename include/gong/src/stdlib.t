--[[
  stdlib - exported Lua interface

  See LICENSE
--]]

local Exports = {}
package.loaded["gong.src.stdlib"] = Exports

local Macro           = require 'gong.src.macro'
local Global          = require 'gong.src.global'
local T               = require 'gong.src.types'
local S               = require 'gong.src.schemata'
local Functions       = require 'gong.src.functions'
local AccStructs      = require 'gong.src.acc_structs'
local AccScan         = require 'gong.src.acc_scan'
local AccBVH          = require 'gong.src.acc_bvh'
local GenAPI          = require 'gong.src.genapi'
local C               = require 'gong.src.c'

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

Exports.NewTable      = S.NewTable


-------------------------------------------------------------------------------
-- Acceleration Index API
-------------------------------------------------------------------------------


Exports.is_spatial_index          = AccStructs.is_spatial_index
Exports.is_scan_index             = AccScan.is_scan_index
Exports.is_bvh_index              = AccBVH.is_bvh_index

Exports.scan_index                = AccScan.scan_index
Exports.bvh_index                 = AccBVH.bvh_index

Exports.scan_scan_traversal       = AccScan.scan_scan_traversal
Exports.bvh_bvh_traversal         = AccBVH.bvh_bvh_traversal

Exports.is_traversal              = AccStructs.is_traversal
Exports.is_scan_scan_traversal    = AccScan.is_scan_scan_traversal
Exports.is_bvh_bvh_traversal      = AccBVH.is_bvh_bvh_traversal


-------------------------------------------------------------------------------
-- Compilation API
-------------------------------------------------------------------------------

Exports.CompileLibrary  = GenAPI.CompileLibrary


-------------------------------------------------------------------------------
-- Other (Assorted)
-------------------------------------------------------------------------------

-- assert?

Exports.Macro         = Macro.NewMacro
Exports.is_macro      = Macro.is_macro
Exports.is_quote      = Macro.is_quote

Exports.Constant      = Global.NewConstant
Exports.is_constant   = Global.is_constant

Exports.Global        = Global.NewGlobal
Exports.is_global     = Global.is_global

Exports.is_function   = Functions.is_function
Exports.is_builtin    = Functions.is_builtin
Exports.is_join       = Functions.is_join

Exports.cassert       = C.assert





