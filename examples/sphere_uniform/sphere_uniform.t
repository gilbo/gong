import 'gong'
local G = gong.stdlib

local num       = G.float

local params = { suffix = '', traversal = 'scan_scan' }
for _,a in ipairs(arg) do
  local _, _, label, value = a:find("^%-([^=]*)=(.*)$")
  if label and value then params[label] = value end
end

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

local AABB3f = G.AABB3f

local gong function Spheres_to_AABB3f( s : Spheres ) : AABB3f
  var r   = {RADIUS,RADIUS,RADIUS}
  return { lo = s.pos-r, hi = s.pos+r }
end

local Spheres_BVH     = G.bvh_index {
  table       = Spheres,
  volume      = AABB3f,
  abstract    = Spheres_to_AABB3f,
  vol_union   = G.AABB3f_union,
  point       = G.AABB3f_midpoint,
}

local BVH_Traversal   = G.bvh_bvh_traversal {
  left        = Spheres_BVH,
  right       = Spheres_BVH,
  vol_isct    = G.AABB3f_isct,
}

if params.traversal == 'scan_scan' then
  -- leave the default traversal
elseif params.traversal == 'bvh_bvh' then
  sphere_self_isct:set_cpu_traversal(BVH_Traversal)
else
  error('unrecognized traversal option: '..params.traversal)
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Export

local API = G.CompileLibrary {
  tables          = {Spheres},
  joins           = {sphere_self_isct},
  c_obj_file      = 'sphere_uniform'..params.suffix..'.o',
  cpp_header_file = 'sphere_uniform'..params.suffix..'.h',
}



