import 'gong'
local G = gong.stdlib

local num       = G.float

------------------------------------------------------------------------------
-- Sim Constants / Parameters

local RADIUS    = G.Constant(num, 1.25)

------------------------------------------------------------------------------
-- Support Functions / Defs

local vec3      = G.vector(num,3)

local swap = G.Macro(function(a, b)
  return gong quote a,b = b,a end
end)

local normed = G.Macro(function(a)
  return gong quote
    var b   = a
    var rn  = num(1f) / G.magnitude(b)
  in rn * b end
end)

------------------------------------------------------------------------------
-- Schema

local Spheres     = G.NewTable('Spheres')
                     :NewField('pos',         vec3)
                     :NewField('isct_count',  G.uint32)


------------------------------------------------------------------------------
-- SubRoutines

local radius      = G.Macro(function(s) return gong `RADIUS end)

local gong function sphere_sphere_test( s0 : Spheres, s1 : Spheres )
  var r0, r1  = radius(s0), radius(s1)
  var r2      = (r0 + r1)*(r0 + r1)
  var d       = s0.pos - s1.pos
  return (+[i] d[i]*d[i]) <= r2
end


------------------------------------------------------------------------------
-- Join

local gong join sphere_self_isct ( s0 : Spheres, s1 : Spheres )
  where s0 ~= s1
  where sphere_sphere_test(s0,s1)
do
  s0.isct_count += 1
  s1.isct_count += 1
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Export

local API = G.CompileLibrary {
  tables          = {Spheres},
  joins           = {sphere_self_isct},
  c_obj_file      = 'sphere_uniform.o',
  cpp_header_file = 'sphere_uniform.h',
}



