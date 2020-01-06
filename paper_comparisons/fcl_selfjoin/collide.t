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


local vec3 = G.vec3f

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
                 :NewField(  'obj_id',  G.uint32)

  return Mesh
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------

local Mesh            = TriMesh()

local ETcontacts      = G.NewTable('ETcontacts')
                        :NewField( 'edge',  Mesh.Edges   )
                        :NewField( 'tri',   Mesh.Tris    )
                        :NewField( 'pos',   vec3 )

if USE_GPU then
  ETcontacts:setGPUSizeLinear(2, Mesh.Edges, 2, Mesh.Tris, 64)
end


local TTcontacts      = G.NewTable('TTcontacts')
                        :NewField( 'obj0',   G.uint32     )
                        :NewField( 'obj1',   G.uint32     )
                        :NewField( 'tri0',   Mesh.Tris    )
                        :NewField( 'tri1',   Mesh.Tris    )
                        :NewField( 'pos',   vec3       )
                        :NewField( 'penetration_depth', G.float)
                        :NewField( 'normal',    vec3 )

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

--[[
template <typename S>
int Intersect<S>::project6(const Vector3<S>& ax,
                        const Vector3<S>& p1, const Vector3<S>& p2, const Vector3<S>& p3,
                        const Vector3<S>& q1, const Vector3<S>& q2, const Vector3<S>& q3)
{
  --]]

local gong function project6( ax : vec3, 
                        p1 : vec3, p2 : vec3, p3 : vec3, 
                        q1 : vec3, q2 : vec3, q3 : vec3) : G.bool
  --[[
  S P1 = ax.dot(p1);
  S P2 = ax.dot(p2);
  S P3 = ax.dot(p3);
  S Q1 = ax.dot(q1);
  S Q2 = ax.dot(q2);
  S Q3 = ax.dot(q3);
  --]]
  var P1 = dot(ax, p1)
  var P2 = dot(ax, p2)
  var P3 = dot(ax, p3)
  var Q1 = dot(ax, q1)
  var Q2 = dot(ax, q2)
  var Q3 = dot(ax, q3)

  --[[
  S mn1 = std::min(P1, std::min(P2, P3));
  S mx2 = std::max(Q1, std::max(Q2, Q3));
  if(mn1 > mx2) return 0;
  --]]
  var mn1 = G.min(P1, G.min(P2, P3))
  var mx2 = G.max(Q1, G.max(Q2, Q3))
  if mn1 > mx2 then return false end

  --[[
  S mx1 = std::max(P1, std::max(P2, P3));
  S mn2 = std::min(Q1, std::min(Q2, Q3));
  if(mn2 > mx1) return 0;
  return 1;
}
  --]]
  var mx1 = G.max(P1, G.max(P2, P3))
  var mn2 = G.min(Q1, G.min(Q2, Q3))
  return (mn2 <= mx1)
end

local squaredNorm = G.Macro(function(a)
  return gong` +[i] a[i] * a[i]
end)

--[[
void normalize(Vector3<S>& v, bool* signal)
{
  S sqr_length = v.squaredNorm();

  if (sqr_length > 0)
  {
    v /= std::sqrt(sqr_length);
    *signal = true;
  }
  else
  {
    *signal = false;
  }
}
--]]
local gong function normalize(v : vec3) : {G.bool,vec3}
  var sqr_length = squaredNorm(v)
  if (sqr_length > 0) then
    return true, v*(1.0/G.sqrt(sqr_length))
  end
  return false,v
end

--[[
bool Intersect<S>::buildTrianglePlane(const Vector3<S>& v1, const Vector3<S>& v2, const Vector3<S>& v3, Vector3<S>* n, S* t)
{
--]]
local gong function buildTrianglePlane(v1 : vec3, v2 : vec3, v3 : vec3) : {G.bool, vec3, G.float}
 
--[[
  Vector3<S> n_ = (v2 - v1).cross(v3 - v1);
  bool can_normalize = false;
  normalize(n_, &can_normalize);
--]]
  var n_ = cross(v2 - v1,v3 - v1);
  var can_normalize,unit_n = normalize(n_)
  
--[[
  if(can_normalize)
  {
    *n = n_;
    *t = n_.dot(v1);
    return true;
  }
  return false;
  --]]
  if can_normalize  then
    return true,unit_n,dot(unit_n,v1)
  end
  return false,n_,0.0
end

--[[
template <typename S>
constexpr S Intersect<S>::getEpsilon()
{
  return 1e-5;
}
--]]
local getEpsilon = G.Macro(function(a)
  return gong`1e-5
end)


--[[
template <typename S>
S Intersect<S>::distanceToPlane(const Vector3<S>& n, S t, const Vector3<S>& v)
{
  return n.dot(v) - t;
}
--]]
local distanceToPlane = G.Macro(function(n,t,v)
  return gong`dot(n,v) - t;
end)


--[[
template <typename S>
void Intersect<S>::computeDeepestPoints(Vector3<S>* clipped_points, unsigned int num_clipped_points, const Vector3<S>& n, S t, S* penetration_depth, Vector3<S>* deepest_points, unsigned int* num_deepest_points)
{
  *num_deepest_points = 0;
  S max_depth = -std::numeric_limits<S>::max();
  unsigned int num_deepest_points_ = 0;
  unsigned int num_neg = 0;
  unsigned int num_pos = 0;
  unsigned int num_zero = 0;

  for(unsigned int i = 0; i < num_clipped_points; ++i)
  {
    S dist = -distanceToPlane(n, t, clipped_points[i]);
    if(dist > getEpsilon()) num_pos++;
    else if(dist < -getEpsilon()) num_neg++;
    else num_zero++;
    if(dist > max_depth)
    {
      max_depth = dist;
      num_deepest_points_ = 1;
      deepest_points[num_deepest_points_ - 1] = clipped_points[i];
    }
    else if(dist + 1e-6 >= max_depth)
    {
      num_deepest_points_++;
      deepest_points[num_deepest_points_ - 1] = clipped_points[i];
    }
  }

  if(max_depth < -getEpsilon())
    num_deepest_points_ = 0;

  if(num_zero == 0 && ((num_neg == 0) || (num_pos == 0)))
    num_deepest_points_ = 0;

  *penetration_depth = max_depth;
  *num_deepest_points = num_deepest_points_;
}
--]]

-- only called with num_clipped_points = 3
local gong function computeDeepestPoints(clipped_points : vec3[3], num_clipped_points : G.uint32, n : vec3, t : G.float) : {G.float, vec3[3], G.uint32}
  var num_deepest_points : G.uint32 = 0
  var deepest_points : vec3[3] = {{0.0f,0.0f,0.0f},{0.0f,0.0f,0.0f},{0.0f,0.0f,0.0f}}
  var max_depth = G.float(-math.huge)
  var num_neg : G.uint32 = 0
  var num_pos : G.uint32 = 0
  var num_zero : G.uint32 = 0

  -- TODO: Is this the right indexing?
  for p = 0,3 do
    var clipped_point : vec3 = :[i] clipped_points[i,p]
    var dist = -distanceToPlane(n, t, clipped_point)
    if dist > getEpsilon() then num_pos = num_pos + 1
    elseif  dist < -getEpsilon() then num_neg = num_neg + 1
    else num_zero = num_zero + 1 end
    if dist > max_depth then
      max_depth = dist;
      num_deepest_points = 1;
      for j=0,3 do
        deepest_points[j,num_deepest_points - 1] = clipped_point[j]
      end
    elseif dist + 1e-6 >= max_depth then
      num_deepest_points = num_deepest_points + 1
      for j=0,3 do
        deepest_points[j,num_deepest_points - 1] = clipped_point[j]
      end
    end
  end

  if max_depth < -getEpsilon() then
    num_deepest_points = 0;
  end

  if (num_zero == 0 and ((num_neg == 0) or (num_pos == 0))) then
    num_deepest_points = 0
  end
  return max_depth, deepest_points, num_deepest_points
end


local gong function tt_is_isct( p : Mesh.Tris, q : Mesh.Tris ) : { vec3, vec3, G.uint32, G.float, vec3 }
  var pos0 : vec3 = {0f,0f,0f}
  var pos1 : vec3 = {0f,0f,0f}
  var num_contact_points : G.uint32 = 0
  var penetration_depth : G.float = 0.0
  var normal : vec3 = {0f,0f,0f}
  --[[
  Vector3<S> p1 = P1 - P1;
  Vector3<S> p2 = P2 - P1;
  Vector3<S> p3 = P3 - P1;
  Vector3<S> q1 = Q1 - P1;
  Vector3<S> q2 = Q2 - P1;
  Vector3<S> q3 = Q3 - P1;
  --]]
  var P1,P2,P3 = p.v[0].pos,p.v[1].pos,p.v[2].pos
  var Q1,Q2,Q3 = q.v[0].pos,q.v[1].pos,q.v[2].pos
  var p1 = P1 - P1
  var p2 = P2 - P1
  var p3 = P3 - P1
  var q1 = Q1 - P1
  var q2 = Q2 - P1
  var q3 = Q3 - P1
--[[
  Vector3<S> e1 = p2 - p1;
  Vector3<S> e2 = p3 - p2;
  Vector3<S> n1 = e1.cross(e2);
  if (!project6(n1, p1, p2, p3, q1, q2, q3)) return false;
--]]

  var e1 = p2 - p1;
  var e2 = p3 - p2;
  var n1 = cross(e1,e2)
  if not project6(n1, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end


--[[
  Vector3<S> f1 = q2 - q1;
  Vector3<S> f2 = q3 - q2;
  Vector3<S> m1 = f1.cross(f2);
  if (!project6(m1, p1, p2, p3, q1, q2, q3)) return false;
--]]
  var f1 = q2 - q1;
  var f2 = q3 - q2;
  var m1 = cross(f1, f2);
  if not project6(m1, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end
  
--[[
  Vector3<S> ef11 = e1.cross(f1);
  if (!project6(ef11, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> ef12 = e1.cross(f2);
  if (!project6(ef12, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> f3 = q1 - q3;
  Vector3<S> ef13 = e1.cross(f3);
  if (!project6(ef13, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> ef21 = e2.cross(f1);
  if (!project6(ef21, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> ef22 = e2.cross(f2);
  if (!project6(ef22, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> ef23 = e2.cross(f3);
  if (!project6(ef23, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> e3 = p1 - p3;
  Vector3<S> ef31 = e3.cross(f1);
  if (!project6(ef31, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> ef32 = e3.cross(f2);
  if (!project6(ef32, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> ef33 = e3.cross(f3);
  if (!project6(ef33, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> g1 = e1.cross(n1);
  if (!project6(g1, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> g2 = e2.cross(n1);
  if (!project6(g2, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> g3 = e3.cross(n1);
  if (!project6(g3, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> h1 = f1.cross(m1);
  if (!project6(h1, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> h2 = f2.cross(m1);
  if (!project6(h2, p1, p2, p3, q1, q2, q3)) return false;

  Vector3<S> h3 = f3.cross(m1);
  if (!project6(h3, p1, p2, p3, q1, q2, q3)) return false;
--]]
  var ef11 = cross(e1,f1);
  if not project6(ef11, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var ef12 = cross(e1,f2);
  if not project6(ef12, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var f3 = q1 - q3;
  var ef13 = cross(e1,f3);
  if not project6(ef13, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var ef21 = cross(e2,f1);
  if not project6(ef21, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var ef22 = cross(e2,f2);
  if not project6(ef22, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var ef23 = cross(e2,f3);
  if not project6(ef23, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var e3 = p1 - p3;
  var ef31 = cross(e3,f1);
  if not project6(ef31, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var ef32 = cross(e3,f2);
  if not project6(ef32, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var ef33 = cross(e3,f3);
  if not project6(ef33, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var g1 = cross(e1,n1);
  if not project6(g1, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var g2 = cross(e2,n1);
  if not project6(g2, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var g3 = cross(e3,n1);
  if not project6(g3, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var h1 = cross(f1,m1);
  if not project6(h1, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var h2 = cross(f2,m1);
  if not project6(h2, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

  var h3 = cross(f3,m1);
  if not project6(h3, p1, p2, p3, q1, q2, q3) then return pos0, pos1, num_contact_points, penetration_depth, normal end

--[[ We always compute intersection information, so this line is  irrelevant
  if(contact_points && num_contact_points && penetration_depth && normal)
--]]
  --[[
      Vector3<S> n1, n2;
      S t1, t2;
      buildTrianglePlane(P1, P2, P3, &n1, &t1);
      buildTrianglePlane(Q1, Q2, Q3, &n2, &t2);
  --]]
    var _,n1,t1 = buildTrianglePlane(P1, P2, P3)
    var _,n2,t2 = buildTrianglePlane(Q1, Q2, Q3)

  --[[
      Vector3<S> deepest_points1[3];
      unsigned int num_deepest_points1 = 0;
      Vector3<S> deepest_points2[3];
      unsigned int num_deepest_points2 = 0;
      S penetration_depth1, penetration_depth2;

      Vector3<S> P[3] = {P1, P2, P3};
      Vector3<S> Q[3] = {Q1, Q2, Q3};
      computeDeepestPoints(Q, 3, n1, t1, &penetration_depth2, deepest_points2, &num_deepest_points2);
      computeDeepestPoints(P, 3, n2, t2, &penetration_depth1, deepest_points1, &num_deepest_points1);
  --]]
    var P : vec3[3] = {P1, P2, P3};
    var Q : vec3[3] = {Q1, Q2, Q3};
    var penetration_depth2, deepest_points2, num_deepest_points2 = computeDeepestPoints(Q, 3, n1, t1)
    var penetration_depth1, deepest_points1, num_deepest_points1 = computeDeepestPoints(P, 3, n2, t2)

  --[[
  if(penetration_depth1 > penetration_depth2)
    {
      *num_contact_points = std::min(num_deepest_points2, (unsigned int)2);
      for(unsigned int i = 0; i < *num_contact_points; ++i)
      {
        contact_points[i] = deepest_points2[i];
      }

      *normal = n1;
      *penetration_depth = penetration_depth2;
    }
    else
    {
      *num_contact_points = std::min(num_deepest_points1, (unsigned int)2);
      for(unsigned int i = 0; i < *num_contact_points; ++i)
      {
        contact_points[i] = deepest_points1[i];
      }

      *normal = -n2;
      *penetration_depth = penetration_depth1;
    }
  }
  --]]
  if penetration_depth1 > penetration_depth2 then
    num_contact_points = num_deepest_points2
    pos0 = :[i] deepest_points2[i,0]
    if num_contact_points > 1 then
      pos1 = :[i] deepest_points2[i,1]
    end
    normal,penetration_depth = n1,penetration_depth2
  else
    num_contact_points = num_deepest_points1
    pos0 = :[i] deepest_points1[i,0]
    if num_contact_points > 1 then
      pos1 = :[i] deepest_points1[i,1]
    end
    normal,penetration_depth = -n2,penetration_depth1
  end

  if num_contact_points > 2 then num_contact_points = 2 end
  
  return pos0, pos1, num_contact_points, penetration_depth, normal
end


local gong function et_is_isct( e : Mesh.Edges, t : Mesh.Tris ) : { G.bool, vec3 }
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
  var pass,pos = et_is_isct(e,t)
  where pass
do
  emit { edge=e, tri=t, pos=pos } in ETcontacts
end

local gong join find_tt_iscts( t0 : Mesh.Tris, t1 : Mesh.Tris )
  var different_objects = t0.obj_id ~= t1.obj_id
  var pos0, pos1, num_contact_points, penetration_depth, normal = tt_is_isct(t0,t1)
  where different_objects and num_contact_points > 0
do
  emit { obj0 = t0.obj_id, obj1 = t1.obj_id, tri0=t0, tri1=t1, pos=pos0, penetration_depth=penetration_depth, normal=normal } in TTcontacts
  if num_contact_points == 2 then
    emit { obj0 = t0.obj_id, obj1 = t1.obj_id, tri0=t0, tri1=t1, pos=pos1, penetration_depth=penetration_depth, normal=normal } in TTcontacts
  end
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

local BVH_TT_Traversal   = G.bvh_bvh_traversal {
  left        = Tris_BVH,
  right       = Tris_BVH,
  vol_isct    = G.AABB3f_isct,
}

find_tt_iscts:set_cpu_traversal(BVH_TT_Traversal)

if params.traversal == 'scan_scan' then
  -- leave the default traversal
elseif params.traversal == 'bvh_bvh' then
  find_et_iscts:set_cpu_traversal(BVH_Traversal)
  find_tt_iscts:set_cpu_traversal(BVH_Traversal)
else
  error('unrecognized traversal option: '..params.traversal)
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------

local API = G.CompileLibrary {
  tables          = {},
  joins           = {find_et_iscts,find_tt_iscts},
  c_obj_file      = 'collide_gong'..params.suffix..'.o',
  cpp_header_file = 'collide_gong'..params.suffix..'.h',
  gpu             = USE_GPU,
}

------------------------------------------------------------------------------
------------------------------------------------------------------------------