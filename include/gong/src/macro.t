
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


local function NewQuote(expr_ast)
  local q = setmetatable({
    _ast = expr_ast,
  }, Quote)
  return q
end
Exports.NewQuote = NewQuote

local function is_quote(obj) return getmetatable(obj) == Quote end
Exports.is_quote = is_quote



function Quote:gettype()
  return self._ast.type
end


