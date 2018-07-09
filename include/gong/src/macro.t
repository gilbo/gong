
local Exports = {}
package.loaded["gong.src.macro"] = Exports

local newlist = terralib.newlist

-------------------------------------------------------------------------------
-- Macros & Quotes
-------------------------------------------------------------------------------

local Macro   = {}
Macro.__index = Macro

local Quote   = {}
Quote.__index = Quote


local function NewMacro(clbk)
  local m = setmetatable({
    _clbk = clbk,
  }, Macro)
  return m
end
Exports.NewMacro = NewMacro

local function is_macro(obj) return getmetatable(obj) == Macro end
Exports.is_macro = is_macro


local function NewQuote(ast, srcinfo, is_stmt)
  local q = setmetatable({
    _ast      = ast,
    _srcinfo  = srcinfo,
    _is_stmt  = is_stmt,
  }, Quote)
  return q
end
Exports.NewQuote = NewQuote

local function is_quote(obj) return getmetatable(obj) == Quote end
Exports.is_quote = is_quote



function Quote:gettype()
  return self._ast.type
end
function Quote:is_statement()
  return self._is_stmt
end
function Quote:is_value()
  return self._ast:is_literal()
end
function Quote:get_value()
  if self._ast:is_literal()
  then
    return self._ast.value
  else
    error('cannot get value of a non-value quote', 2)
  end
end






