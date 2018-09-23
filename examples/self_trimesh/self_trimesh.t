import 'gong'
local G = gong.stdlib

local EPSILON         = 1.0e-7

-- quick hack for control
local USE_GPU = not not terralib.cudacompile
print('USE GPU? ', USE_GPU)

local params = { suffix = '', traversal = 'scan_scan' }
for _,a in ipairs(arg) do
  local _, _, label, value = a:find("^%-([^=]*)=(.*)$")
  if label and value then params[label] = value end
end

------------------------------------------------------------------------------

local function TriMesh()
  local Mesh          = {}
  local pos_type      = G.float
  local pttype        = pos_type:terratype()

  -- Declare basic schema
  Mesh.Verts  = G.NewTable('Verts')
                 :NewField( 'pos',      G.vector(pos_type, 3) )
  Mesh.Edges  = G.NewTable('Edges')
                 :NewField( 'hd',       Mesh.Verts   )
                 :NewField( 'tl',       Mesh.Verts   )
  Mesh.Tris   = G.NewTable('Tris')
                 :NewField(  'v',       G.vector(Mesh.Verts, 3) )

  return Mesh
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------

local Mesh            = TriMesh()

local ETcontacts      = G.NewTable('ETcontacts')
                        :NewField( 'edge',  Mesh.Edges   )
                        :NewField( 'tri',   Mesh.Tris    )
                        :NewField( 'pos',   G.vec3f )

if USE_GPU then
  ETcontacts:setGPUSizeLinear(2, Mesh.Edges, 2, Mesh.Tris, 64)
end

------------------------------------------------------------------------------

local cross = G.Macro(function(a,b)
  local typ = a:gettype()
  return gong quote
    var aa = a
    var bb = b
  in {
    aa[1] * bb[2] - aa[2] * bb[1],
   -aa[0] * bb[2] + aa[2] * bb[0],
    aa[0] * bb[1] - aa[1] * bb[0]
  } end
end)
local dot = G.Macro(function(a,b)
  return gong` +[i] a[i] * b[i]
end)

local gong function et_is_isct( e : Mesh.Edges, t : Mesh.Tris ) : { G.bool, G.vec3f }
  var v0  = t.v[0].pos
  var v1  = t.v[1].pos
  var v2  = t.v[2].pos
  var ee  = e.hd.pos - e.tl.pos
  var eo  = e.tl.pos

  var e1  = v1 - v0
  var e2  = v2 - v0

  var h   = cross(ee, e2)
  var a   = dot(e1, h)
  if a > -EPSILON and a < EPSILON then    return false, {0.f,0.f,0.f}   end
  var f   = 1.0f/a
  var s   = eo - v0
  var u   = f * dot(s,h)
  if u < 0.0f or u > 1.0f then            return false, {0.f,0.f,0.f}   end
  var q   = cross(s, e1)
  var v   = f * dot(ee, q)
  if v < 0.0f or u + v > 1.0f then        return false, {0.f,0.f,0.f}   end
  var t   = f * dot(e2, q)
  if t <= EPSILON or t >= 1.0f - EPSILON then return false, {0.f,0.f,0.f} end
  -- otherwise
  var pos = (1.0f-t)*e.tl.pos + t*e.hd.pos
  return true, pos
end

local gong join find_et_iscts( e : Mesh.Edges, t : Mesh.Tris )
  var pass,pos    = et_is_isct(e,t)
  where pass
do
  emit { edge=e, tri=t, pos=pos } in ETcontacts
end

------------------------------------------------------------------------------

local AABB3f = G.AABB3f

local gong function Edges_to_AABB3f( e : Mesh.Edges ) : AABB3f
  return { lo = :[i] G.min(e.hd.pos[i], e.tl.pos[i]), 
           hi = :[i] G.max(e.hd.pos[i], e.tl.pos[i]) }
end
local gong function Tris_to_AABB3f( t : Mesh.Tris ) : AABB3f
  return { lo = :[i] :min[j] t.v[j].pos[i],
           hi = :[i] :max[j] t.v[j].pos[i] }
end


local Edges_BVH       = G.bvh_index {
  table       = Mesh.Edges,
  volume      = AABB3f,
  abstract    = Edges_to_AABB3f,
  vol_union   = G.AABB3f_union,
  point       = G.AABB3f_midpoint,
}
local Tris_BVH        = G.bvh_index {
  table       = Mesh.Tris,
  volume      = AABB3f,
  abstract    = Tris_to_AABB3f,
  vol_union   = G.AABB3f_union,
  point       = G.AABB3f_midpoint,
}

local BVH_Traversal   = G.bvh_bvh_traversal {
  left        = Edges_BVH,
  right       = Tris_BVH,
  vol_isct    = G.AABB3f_isct,
}

if params.traversal == 'scan_scan' then
  -- leave the default traversal
elseif params.traversal == 'bvh_bvh' then
  find_et_iscts:set_cpu_traversal(BVH_Traversal)
else
  error('unrecognized traversal option: '..params.traversal)
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------

local API = G.CompileLibrary {
  tables          = {},
  joins           = {find_et_iscts},
  c_obj_file      = 'self_trimesh'..params.suffix..'.o',
  cpp_header_file = 'self_trimesh'..params.suffix..'.h',
  gpu             = USE_GPU,
}

------------------------------------------------------------------------------
------------------------------------------------------------------------------

