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
--Tris:NewField('v',   G.vector(Verts, 3), { keyrep = keyT })
Tris:NewField('v',   G.vector(Verts, 3))

local BoolIntersections = G.NewTable('BoolIntersections')
							:NewField('ray', Rays)
							:SetPrimaryKey('ray')

--local BoolIntersections = G.NewTable('BoolIntersections'):NewField('ray', Rays)

local RayTriIntersections = G.NewTable('RayTriIntersections')
						:NewField('ray', Rays)
						:NewField('tri', Tris)

local FullIntersections = G.NewTable('FullIntersections')
						:NewField('ray', Rays)
						:NewField('tri', Tris)
						:NewField('barys', vec3)
						:NewField('t', floattype)



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
local gong function is_isct_raytri( ray : Rays, tri : Tris ) : {G.bool, vec3, G.float}
    var EPSILON : G.float = 0.0000001; 
    var edge1 = tri.v[1].pos - tri.v[0].pos;
    var edge2 = tri.v[2].pos - tri.v[0].pos;
    var h = G.cross(ray.direction,edge2)
    var a = G.dot(edge1,h);


    if a > -EPSILON and a < EPSILON then
        return false, {0.f,0.f,0.f}, 0.0
    end
    var f = 1/a
    var s = ray.origin - tri.v[0].pos
    var u = f * G.dot(s,h)
    if u < 0.0 or u > 1.0 then
        return false, {u,0.f,0.f}, 0.0
    end
    var q = G.cross(s,edge1)
    var v = f * G.dot(ray.direction,q)
    if (v < 0.0 or (u + v) > 1.0) then
        return false, {u,v,1.0-u-v}, 0.0
    end
    var t = f * G.dot(edge2,q);
    if (t > EPSILON and t < ray.tMax) then
        return true, {u,v,1.0-u-v}, t
    else 
        return false, {u,v,1.0-u-v}, t
    end
end

local gong join find_raytri_iscts_bool( r : Rays, t : Tris )
  var hit,barys,t_dist = is_isct_raytri(r,t)
  where hit
do
	merge x in BoolIntersections do
	    --x.count = x.count + 1
	else emit { ray=r } in BoolIntersections end
end

local gong join find_raytri_iscts( r : Rays, t : Tris )
  var hit,barys,t_dist = is_isct_raytri(r,t)
  where hit
do
  emit { ray=r, tri=t } in RayTriIntersections
end

local gong join find_raytri_iscts_full( r : Rays, tr : Tris )
  var hit,bs,t_dist = is_isct_raytri(r,tr)
  where hit
do
  emit { ray=r, tri=tr, barys=bs, t=t_dist } in FullIntersections
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------

local API = G.CompileLibrary {
  tables          = {},
  joins           = {find_raytri_iscts, find_raytri_iscts_full,find_raytri_iscts_bool},
  c_obj_file      = 'raycaster_gong.o',
  cpp_header_file = 'raycaster_gong.h',
}

------------------------------------------------------------------------------
------------------------------------------------------------------------------

