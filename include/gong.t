-- File to be imported, defining the language
-- put import 'gong' at the top of files

local P           = require 'gong.src.parser'
local Specializer = require 'gong.src.specializer'
local TC          = require 'gong.src.typechecker'
--local F           = require 'gong.src.functions'
local StdLib      = require 'gong.src.stdlib'
-- other passes and things?

local function handleStatement(self, lexer)
  local ast, assigntuple = P.ParseStatement(lexer)
  local constructor = function(env_fn)
    if StdLib._UNIT_TEST_PARSER then
      return ast
    elseif StdLib._UNIT_TEST_SPECIALIZER then
      return Specializer.specialize(ast, env_fn())
    else
      local spec  = Specializer.specialize(ast, env_fn())
      local tcast = TC.typecheck(spec)
      return tcast
--    else
--      local decl_ast = Specializer.specialize(ast, env_fn())
--      return F.NewFunction { decl_ast = decl_ast }
    end
  end
  return constructor, assigntuple
end

local function get_library(self, lexer)
  lexer:expect('gong')
  lexer:expect('.')
  if lexer:expect(lexer.name).value == 'stdlib' then
    return function(env_fn)
      return StdLib
    end
  else
    lexer:error("expected 'stdlib'")
  end
end

local function handleExpression(self, lexer)
  if lexer:matches('gong') and lexer:lookahead().type == '.' then
    return get_library(self, lexer)
  end
  local ast = P.ParseExpression(lexer)
  local constructor = function(env_fn)
    if StdLib._UNIT_TEST_PARSER then
      return ast
    elseif StdLib._UNIT_TEST_SPECIALIZER then
      return Specializer.specialize(ast, env_fn())
    else
      local spec  = Specializer.specialize(ast, env_fn())
      local tcast = TC.typecheck(spec)
      return tcast
--    else
--      local decl_ast = Specializer.specialize(ast, env_fn())
--      return F.NewFunction { decl_ast = decl_ast }
    end
  end
  return constructor
end


local gong_language = {
  name          = 'gong',
  entrypoints   = {'gong'},
  keywords      = {
  --  '_', -- always good to reserve the underscore for language use
    'var',
    'let',
    'where',
    'emit',
  },

  expression      = handleExpression,
  statement       = handleStatement,
  localstatement  = handleStatement,
}

return gong_language
