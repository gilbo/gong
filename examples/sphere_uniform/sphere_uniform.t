import 'gong'
local G = gong.stdlib

local num       = G.float

-- quick hack for control
local USE_GPU = not not terralib.cudacompile
print('USE GPU? ', USE_GPU)

local params = { suffix = '', traversal = 'scan_scan' }
for _,a in ipairs(arg) do
  local _, _, label, value = a:find("^%-([^=]*)=(.*)$")
  if label and value then params[label] = value end
end

------------------------------------------------------------------------------
-- Sim Constants / Parameters

local RADIUS    = G.Constant(num, 1.0)

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

local Contacts    = G.NewTable('Contacts')
                     :NewField('s0', Spheres)
                     :NewField('s1', Spheres)

if USE_GPU then
  Contacts:setGPUSizeLinear(8, Spheres, 64)
end

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
  emit { s0=s0, s1=s1 } in Contacts
end
--sphere_self_isct:buffer_index_on_cpu()

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

local CELL_WIDTH = gong `4f * RADIUS * 1.1f
local gong function Spheres_to_DilatedGridRange( s : Spheres )
  -- floating point bounding box...
  var r     = 2f*{RADIUS, RADIUS, RADIUS}
  var lo    = s.pos-r
  var hi    = s.pos+r
  -- grid cell width
  var w     = CELL_WIDTH
  var inv_w = 1.0f / w
  var lo_i  = :[i] G.int32( G.floor( lo[i] * inv_w ) )
  var hi_i  = :[i] G.int32( G.floor( hi[i] * inv_w ) )
  return lo_i, hi_i
end

local gong function Spheres_to_Point( s : Spheres )
  -- grid cell width
  var w       = CELL_WIDTH
  var inv_w   = 1.0f / w
  return :[i] G.int32( G.floor( s.pos[i] * inv_w ) )
end

local hash_function = G.hash3i
if params.traversal == 'grid_scan' then
  local factor = 4
  local gong function grid_hash( k : G.vec3i )
    var x = G.uint32(  ((k[0] % 80) + 80) % 80  )
    var y = G.uint32(  ((k[1] % 80) + 80) % 80  )
    return x*80 + y
  end
  hash_function = grid_hash
end

local DilatedHashGrid   = G.hash_index {
  table       = Spheres,
  key         = G.vec3i,
  abs_point   = Spheres_to_Point,
  hash        = hash_function,
}

local Hash_Traversal  = G.scan_hash_traversal {
  left              = DilatedHashGrid,
  right             = Spheres,
  right_abs_range   = Spheres_to_DilatedGridRange,
}


if params.traversal == 'scan_scan' then
  -- leave the default traversal
elseif params.traversal == 'bvh_bvh' then
  sphere_self_isct:set_cpu_traversal(BVH_Traversal)
  sphere_self_isct:set_gpu_traversal(BVH_Traversal)
elseif params.traversal == 'hash_scan' then
  sphere_self_isct:set_cpu_traversal(Hash_Traversal)
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
  gpu             = USE_GPU,
}



