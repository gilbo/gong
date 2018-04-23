import 'gong'

local B = {}
package.loaded["gong.src.builtin"] = B

local T               = require 'gong.src.types'
local Util            = require 'gong.src.util'
local E               = require 'gong.src.effectcheck'
--local is_type         = T.is_type
--local SrcInfo         = Util.SrcInfo
--local NewSymbol       = Util.NewSymbol
--local is_symbol       = Util.is_symbol
--local is_id_str       = Util.is_id_str
--local is_int          = Util.is_int
local INTERNAL_ERR    = Util.INTERNAL_ERR

local Schemata        = require 'gong.src.schemata'
local Macro           = require 'gong.src.macro'
local Functions       = require 'gong.src.functions'
local is_function     = Functions.is_function
local is_builtin      = Functions.is_builtin
local NewBuiltIn      = Functions.NewBuiltIn
local NewMacro        = Macro.NewMacro

local C               = require 'gong.src.c'

local newlist         = terralib.newlist

-------------------------------------------------------------------------------
-- Support
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- Builtin Functions
-------------------------------------------------------------------------------

--
B.assert  = NewBuiltIn{
  name        = 'assert',
  luafunc     = assert,
  typecheck   = function(args,ast,ctxt) error('assert unimplemented')
    end,
  effectcheck = function(args,ast,ctxt) error('assert unimplemented')
    end,
  codegen     = function(args,ast,ctxt) error('assert unimplemented')
    end,
}--]]


local function unary_arg_builtin(nm)
  local cpu_fn    = assert(C[nm])

  local b         = NewBuiltIn {
    name = nm,
    typecheck = function(args,ast,ctxt)
        if #args ~= 1 then
          ctxt:error(ast, nm.." expects exactly 1 argument "..
                              "(instead got "..#args..")")
          return T.error
        end
        local typ = args[1].type
        if not typ:is_numeric() or not typ:is_primitive() then
          ctxt:error(args[1], "argument to "..name.." must be a number")
          return T.error
        end
        -- otherwise, work out return type
        if      typ:is_coercable_to(T.float) then return T.float
        elseif  typ:is_coercable_to(T.double) then return T.double
        else
          ctxt:error(args[1], "argument to "..name.." could not "..
                              "be coerced into a double")
          return T.error
        end
      end,
    effectcheck = function() return newlist() end,
    codegen     = function(args, ast, ctxt)
        local a         = args[1]
        local ttype     = ast.type:terratype()
        return `[ttype](cpu_fn(a))
      end,
  }

  return b
end

local function binary_arg_builtin(nm)
  local cpu_fn    = assert(C[nm])

  local b         = NewBuiltIn {
    name = nm,
    typecheck = function(args,ast,ctxt)
        if #args ~= 2 then
          ctxt:error(ast, nm.." expects exactly 2 argument "..
                              "(instead got "..#args..")")
          return T.error
        end

        local rettype = T.float
        for i,a in ipairs(args) do
          local typ = a.type
          if not typ:is_numeric() or not typ:is_primitive() then
            ctxt:error(a, "argument to "..name.." must be a number")
            return T.error
          end
          -- otherwise, work out return type
          if      typ:is_coercable_to(rettype)  then
            rettype = rettype
          elseif  typ:is_coercable_to(T.double) then
            rettype = T.double
          else
            ctxt:error(args[1], "argument to "..name.." could not "..
                                "be coerced into a double")
            return T.error
          end
        end

        return rettype
      end,
    effectcheck = function() return newlist() end,
    codegen     = function(args, ast, ctxt)
        local a0, a1    = args[1], args[2]
        local ttype     = ast.type:terratype()
        return `[ttype](cpu_fn(a0,a1))
      end,
  }

  return b
end

B.cos   = unary_arg_builtin('cos')
B.acos  = unary_arg_builtin('acos')
B.sin   = unary_arg_builtin('sin')
B.asin  = unary_arg_builtin('asin')
B.tan   = unary_arg_builtin('tan')
B.atan  = unary_arg_builtin('atan')
B.sqrt  = unary_arg_builtin('sqrt')
B.cbrt  = unary_arg_builtin('cbrt')
B.floor = unary_arg_builtin('floor')
B.ceil  = unary_arg_builtin('ceil')
B.log   = unary_arg_builtin('log')
B.log2  = unary_arg_builtin('log2')
B.exp   = unary_arg_builtin('exp')
B.exp2  = unary_arg_builtin('exp2')

B.atan2 = binary_arg_builtin('atan2')



B.abs = NewBuiltIn {
  name          = 'abs',
  typecheck     = function(args,ast,ctxt)
      if #args ~= 1 then
        ctxt:error(ast, "abs expects exactly 1 argument "..
                            "(instaed got "..#args..")")
        return T.error
      end
      local typ = args[1].type
      if not typ:is_numeric() or not typ:is_primitive() then
        ctxt:error(args[1], "argument to abs must be a number")
        return T.error
      end
      return typ
    end,
  effectcheck   = function() return newlist() end,
  codegen       = function(args,ast,ctxt)
      local typ   = ast.args[1].type
      local a     = args[1]
      if not typ:is_signed() then return a
      else
        return quote var x = [a] in terralib.select(x<0,-x,x) end
      end
    end,
}



local function printSingle(typ, e, args)
  if      typ:is_row() then
    args:insert(e)
    return "%u"
  elseif  typ:is_internal() then
    assert(type(typ.value) == 'string')
    args:insert(e)
    return "%s"
  elseif typ == T.bool then
    args:insert(`terralib.select(e, "true ", "false"))
    return "%s"
  elseif  typ:is_float() then
    args:insert(e)
    return "%f"
  elseif  typ:is_signed() then
    args:insert(e)
    if typ == T.int64 then  return "%ld"
                      else  return "%d" end
  else
    args:insert(e)
    if typ == T.uint64 or typ == T.size64 then  return "%lu"
                                          else  return "%u" end
  end
end

local function buildPrintSpec(typ, e, print_spec, print_args, defs)

  if typ:is_primitive() or typ:is_row() or typ:is_internal() then
    print_spec:insert( printSingle(typ, e, print_args) )
  elseif typ:is_pure_tensor() then
    local dims  = typ.dims
    typ         = typ:basetype()
    if #dims == 1 then
      print_spec:insert('{')
      defs:insert(quote
        var vec = [e]
        escape for i=1,dims[1] do
          if i > 1 then print_spec:insert(',') end
          local expr = (`vec.d[i-1])
          print_spec:insert( printSingle(typ, expr, print_args) )
        end end
      end)
      print_spec:insert('}')
    else assert(#dims == 2)
      print_spec:insert('{')
      local N = dims[1]
      defs:insert(quote
        var mat = [e]
        escape for j=1,dims[2] do
          if j > 1 then print_spec:insert(';') end
          for i=1,N do
            if i > 1 then print_spec:insert(',') end
            local expr = (`mat.d[ [(i-1) + (j-1)*N] ])
            print_spec:insert( printSingle(typ, expr, print_args) )
        end end end
      end)
      print_spec:insert('}')
    end
  else
    error("Unrecognized type in print: "..tostring(typ))
  end
end

B.print = NewBuiltIn {
  name          = 'print',
  typecheck     = function(args,ast,ctxt)
      for i,out in ipairs(args) do
        local typ   = out.type
        if typ == T.error then
        elseif typ:is_internal() and type(typ.value) == 'string' then
          -- strings are great!
        elseif not typ:is_value() then
          ctxt:error(ast, "only strings and values can be printed, not "..
                     tostring(typ))
        elseif typ:is_record() then
          ctxt:error(ast, "record printing unsupported for "..
                     tostring(typ))
        elseif typ:is_tensor() then
          if not typ:is_pure_tensor() then
            ctxt:error(ast, "tensor printing only supported for tensors of "..
                            "primitives, not "..tostring(typ))
          elseif #typ.dims >= 3 then
            ctxt:error(ast, "tensor printing only supported for "..
                            "vectors and matrices, not "..
                            tostring(typ))
          end
        end
      end
      return nil -- this should be a statement, in other words
    end,
  effectcheck   = function(args,ast,ctxt)
      -- return a printing effect
      return newlist{ E.Effects.Print(ast.srcinfo) }
    end,
  codegen       = function(args,ast,ctxt)
      local print_spec, print_args, defs = newlist(), newlist(), newlist()
      for i,a in ipairs(args) do
        if i > 1 then print_spec:insert(' ') end
        buildPrintSpec(ast.args[i].type, a, print_spec, print_args, defs)
      end
      print_spec:insert('\n')
      local fmtstr = print_spec:concat('')

      local printf = C.printf
      return quote [defs]; printf(fmtstr, print_args) end
    end,
}


-- request list:

-- B.print
-- B.rand()
-- B.pow(base,exp)



-------------------------------------------------------------------------------
-- Builtin Macros
-------------------------------------------------------------------------------

B.dot = NewMacro(function(x,y)
  return gong quote
    var a = x
    var b = y
  in +[i] a[i]*b[i] end
end)
B.cross = NewMacro(function(x,y)
  return gong quote
    var a = x
    var b = y
  in { a[1]*b[2] - a[2]*b[1],
      -a[0]*b[2] + a[2]*b[0],
       a[0]*b[1] - a[1]*b[0] } end
end)
B.magnitude = NewMacro(function(x)
  return gong quote
    var a = x
  in B.sqrt(+[i] a[i]*a[i]) end
end)

B.min = NewMacro(function(x,y)
  return gong quote
    var a = x
    var b = y
  in (a < b)? a else b end
end)
B.max = NewMacro(function(x,y)
  return gong quote
    var a = x
    var b = y
  in (a > b)? a else b end
end)

B.clamp = NewMacro(function(x_, lo_, hi_)
  return gong quote
    var x   = x_
    var lo  = lo_
    var hi  = hi_
  in (x < lo)? lo else (x > hi)? hi else x end
end)




