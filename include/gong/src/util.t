--[[
  util - Convenience functions

  See LICENSE
--]]

import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.util"] = Exports

local newlist = terralib.newlist

-------------------------------------------------------------------------------
-- Memoization
-------------------------------------------------------------------------------

local niltoken      = {}
local lookuptoken   = {}
local function nilconvert(val) return val==nil and niltoken or val end

-- function must return exactly one value
-- function must take at least idx_arguments
local function memoize_from(idx,f)
  local cache = {}
  local function memoization_wrapper(...)
    local args     = {select(idx,...)}
    local subcache = cache
    for i=1,#args do
      local sub = subcache[nilconvert(args[i])]
      if not sub then
        sub = {}
        subcache[nilconvert(args[i])] = sub
      end
      subcache = sub
    end
    local lookup = subcache[lookuptoken]
    if not lookup then
      lookup = f(...)
      subcache[lookuptoken] = lookup
    end
    return lookup
  end
  return memoization_wrapper
end
local function memoize(f)
  return memoize_from(1,f)
end

local function memoize_named(keys, f)
  local cache = {}
  local function named_memoization_wrapper(args)
    local subcache = cache
    for i=1,#keys-1 do
      local keyval  = nilconvert( args[keys[i]] )
      local lookup  = subcache[keyval]
      if not lookup then
        lookup = {}; subcache[keyval] = lookup
      end
      subcache = lookup
    end
    local keyval  = nilconvert( args[keys[#keys]] )
    local val     = subcache[keyval]
    if not val then
      val = f(args); subcache[keyval] = val
    end
    return val
  end
  return named_memoization_wrapper
end

local memolist_helper = memoize(function(...)
  local xs = newlist()
  local N  = select('#',...)
  for i=1,N do xs[i] = select(i,...) end
  return xs
end)
local function memolist(xs) return memolist_helper(unpack(xs)) end

Exports.memoize       = memoize
Exports.memoize_from  = memoize_from
Exports.memoize_named = memoize_named
Exports.memolist      = memolist


-------------------------------------------------------------------------------
-- Error Reporting / Attributing Support
-------------------------------------------------------------------------------

local ADT A
  SrcInfo = { linenumber  : number,
              offset      : number,
              filename    : string }
end

function A.SrcInfo:__tostring()
  return self.filename..':'..tostring(self.linenumber)
end

Exports.SrcInfo = A.SrcInfo


-------------------------------------------------------------------------------
-- Definition of valid identifier strings
-------------------------------------------------------------------------------

local function is_id_str(obj)
  return type(obj) == 'string' and string.match(obj,'^[%a_][%w_]*$')
end
Exports.is_id_str = is_id_str

local function is_int(obj)
  return type(obj) == 'number' and obj%1 == 0
end
Exports.is_int = is_int

local function is_nat(obj)
  return is_int(obj) and obj >= 0
end
Exports.is_nat = is_nat

local function is_pos_int(obj)
  return is_int(obj) and obj >= 1
end
Exports.is_pos_int = is_pos_int


-------------------------------------------------------------------------------
-- Symbol Objects
-------------------------------------------------------------------------------

local Symbol              = {}
Symbol.__index            = Symbol

local sym_id_count = 0
local function NewSymbol(name)
  assert(name == nil or is_id_str(name),
         'named symbols must use valid identifiers')
  sym_id_count    = sym_id_count + 1
  return setmetatable({
    _namestr = name or '_'..tostring(sym_id_count),
    _id      = sym_id_count,
    _isnamed = not not name,
  }, Symbol)
end
local function is_symbol(obj)
  return getmetatable(obj) == Symbol
end

function Symbol:UniqueCopy()
  return NewSymbol(self._namestr)
end
function Symbol:__tostring()
  return self._namestr
end
function Symbol:uniquestr()
  return '$'..self._id..'_'..self._namestr
end
function Symbol:isnamed()
  return self._isnamed
end

Exports.NewSymbol = NewSymbol
Exports.is_symbol = is_symbol


-------------------------------------------------------------------------------
-- List Functions
-------------------------------------------------------------------------------

local TerraList = getmetatable(terralib.newlist())

function TerraList:copy()
  local function identity(x)
    return x
  end
  return self:map(identity)
end

function TerraList:find(x, pred)
  pred = pred or function(a,b) return a == b end
  for i,elem in ipairs(self) do
    if pred(elem, x) then
      return i
    end
  end
  return nil
end

function TerraList:reverse()
  local res = terralib.newlist()
  for i = #self, 1, -1 do
    res:insert(self[i])
  end
  return res
end

--[[

SHOULDN'T REMOVE DO THESE?

function TerraList:pop()
  local res = self[#self]
  self[#self] = nil
  return res
end

function TerraList:popFront()
  local res = self[1]
  for i = 2,#self do
    self[i-1] = self[i]
  end
  self[#self] = nil
  return res
end
--]]

-------------------------------------------------------------------------------
-- Table Functions
-------------------------------------------------------------------------------

local function shallowCopy(tab)
  local copy = {}
  for k,v in pairs(tab) do
    copy[k] = v
  end
  return copy
end
Exports.shallowCopy = shallowCopy

-- From the Lua Documentation
local function pairs_sorted(tbl, compare)
  local arr = {}
  for k in pairs(tbl) do table.insert(arr, k) end
  table.sort(arr, compare)

  local i = 0
  local iter = function() -- iterator
    i = i + 1
    if arr[i] == nil then return nil
    else return arr[i], tbl[arr[i]] end
  end
  return iter
end
Exports.pairs_sorted = pairs_sorted


