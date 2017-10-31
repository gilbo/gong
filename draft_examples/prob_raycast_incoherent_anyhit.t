import 'gong'

--[[
Start of a simple trimesh raycast example

--]]
local G               = (gong.stdlib)

------------------------------------------------------------------------------
-- Declarative Specification absent loading data

local keyT            = G.uint32
local Rays         = G.NewTable('Rays')
Rays:NewField('origin', G.vec3f)
Rays:NewField('direction', G.vec3f)
Rays:NewField('tMax', G.float)
local Verts        = G.NewTable('Verts')
local Tris         = G.NewTable('Tris')

Verts:NewField('pos', G.vec3f)
Tris:NewField('v',   G.vector(Verts, 3), { keyrep = keyT })
local RTIntersections      = G.NewTable('RTIntersections', {
                          join_policy = 'rebuild',
                          -- i.e. destroy and rebuild the table
                          -- every time a join emits into it.
                        })
RTIntersections:NewField('ray', Rays)
RTIntersections:NewField('tri', Tris)

-- Möller–Trumbore ray-triangle intersection algorithm
local gong function is_isct_raytri( ray : Rays, tri : Tris ) : Bool
    var EPSILON : G.float = 0.0000001; 
    Vector3D vertex0 = inTriangle->vertex0;
    Vector3D vertex1 = inTriangle->vertex1;  
    Vector3D vertex2 = inTriangle->vertex2;
    Vector3D edge1, edge2, h, s, q;
    float a,f,u,v;
    var edge1 = tri.v[1] - tri.v[0];
    var edge2 = tri.v[2] - tri.v[0];
    var h = G.cross(ray.direction,edge2)
    var a = G.dot(edge1,h);
    if a > -EPSILON and a < EPSILON then
        return false
    end
    var f = 1/a;
    var s = ray.origin - tri.v[0];
    var u = f * G.dot(edge1,h);;
    if u < 0.0 or u > 1.0 then
        return false
    end
    var q = G.cross(s,edge1)
    var v = f * G.cross(ray.direction,q)
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

local gong join find_raytri_iscts ()
  r <- Rays
  t <- Tris
  where is_isct_raytri(r,t)
do
  emit { ray=t, tri=t } in RTIntersections
end