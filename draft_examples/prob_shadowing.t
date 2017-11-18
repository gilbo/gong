import 'gong'

local G               = (gong.stdlib)


------------------------------------------------------------------------------
-- Declarative Specification absent loading data

local keyT            = G.uint32

local RayEdges        = G.NewTable('RayEdges')
local Particles       = G.NewTable('Particles')

RayEdges:NewField('p0', Particles)
RayEdges:NewField('p1', Particles)
Particles:NewField('pos', G.vec3f)
RayEdges:NewField('is_shadowed', G.bool)

local particle_diameter
                      = G.Global('particle_diameter', G.float, 1.0e-3)

local EPS             = 1e-7

local gong function is_occluding( re : RayEdges, p : Particles ) : G.bool
  var pp    = p.pos
  var p0    = re.p0.pos
  var p1    = re.p1.pos

  -- strategy: the three points form a triangle, with re as a base.
  --           part of our question is about the height of that triangle
  --           as the direction orthogonal to the base.  Consequently...
  --    H = A / B   (where A is twice the triangle's area)
  -- We can compute the area
  var e     = p1-p0
  var pe    = pp-p0
  var A     = G.fabs(G.cross(e, pe))
  var B     = G.magnitude(e)
  -- is H < DIAM ?   A / B < DIAM ?   A < B * DIAM
  if A > B * particle_diameter then return false end

  -- the other part of our question is whether the point lies
  -- in-between the two other points; determined now by projection
  var scale = (B < EPS)? 1.0f/EPS : 1.0f/B
  var t     = scale * G.dot(e,pe)
  if t < EPS or t > 1.0f-EPS then return false else return true end
end



local gong join find_et_iscts ()
  re <- RayEdges
  p  <- Particles
  where re.p0 ~= p and re.p1 ~= p
  where is_occluding(re,p)
do
  r.is_shadowed or= true
end


