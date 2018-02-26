import 'gong'
local G = gong.stdlib

------------------------------------------------------------------------------
-- Declarative Specification absent loading data

local floattype = G.float
local vec3      = G.vector(floattype,3)


local keyT            = G.uint32
local Rays         = G.NewTable('Rays')
Rays:NewField('origin',     vec3)
Rays:NewField('direction',  vec3)
Rays:NewField('tMax',       floattype)
local Verts        = G.NewTable('Verts')
local Tris         = G.NewTable('Tris')

Verts:NewField('pos', vec3)
Tris:NewField('v',   G.vector(Verts, 3), { keyrep = keyT })
local RTIntersections      = G.NewTable('RTIntersections', {
                          join_policy = 'rebuild',
                          -- i.e. destroy and rebuild the table
                          -- every time a join emits into it.
                        })
RTIntersections:NewField('ray', Rays)
RTIntersections:NewField('tri', Tris)

------------------------------------------------------------------------------
-- TODO: move into gong proper?
G.cross = G.Macro(function(a,b)
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
G.dot = G.Macro(function(a,b)
  return gong` +[i] a[i] * b[i]
end)
------------------------------------------------------------------------------

-- Möller–Trumbore ray-triangle intersection algorithm
local gong function is_isct_raytri( ray : Rays, tri : Tris ) : G.bool
    var EPSILON : G.float = 0.0000001; 
    var edge1 = tri.v[1].pos - tri.v[0].pos;
    var edge2 = tri.v[2].pos - tri.v[0].pos;
    var h = G.cross(ray.direction,edge2)
    var a = G.dot(edge1,h);
    if a > -EPSILON and a < EPSILON then
        return false
    end
    var f = 1/a;
    var s = ray.origin - tri.v[0].pos
    var u = f * G.dot(edge1,h)
    if u < 0.0 or u > 1.0 then
        return false
    end
    var q = G.cross(s,edge1)
    var v = f * G.dot(ray.direction,q)
    if (v < 0.0 or (u + v) > 1.0) then
        return false
    end
    var t = f * G.dot(edge2,q);
    if (t > EPSILON and t < ray.tMax) then
        return true;
    else 
        return false;
    end
end

local gong join find_raytri_iscts( r : Rays, t : Tris )
  var hit = is_isct_raytri(r,t)
  where hit
do
  emit { ray=r, tri=t } in RTIntersections
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------

local API = G.CompileLibrary {
  tables          = {},
  joins           = {find_raytri_iscts},
  c_obj_file      = 'raycaster_gong.o',
  cpp_header_file = 'raycaster_gong.h',
}

------------------------------------------------------------------------------
------------------------------------------------------------------------------

