import 'gong'

local G               = (gong.stdlib)


------------------------------------------------------------------------------
-- Declarative Specification absent loading data

local keyT            = G.uint32

local ShadowRays      = G.NewTable('ShadowRays')
local Verts           = G.NewTable('Verts')
local Tris            = G.NewTable('Tris')

ShadowRays:NewField('origin', G.vec3f)
--ShadowRays:NewField('light',  G.vec3f)
Verts:NewField('pos', G.vec3f)
Tris:NewField('v',   G.vector(Verts, 3), { keyrep = keyT })

local lightpos        = G.Global(G.vec3f, {0,0,0})
local EPS             = 1e-7


local gong function ray_tri( ray : ShadowRays, t : Tris ) : Bool
  var r0  = ray.origin
  var r1  = lightpos

  var p0  = t.v[0].pos
  var p1  = t.v[1].pos
  var p2  = t.v[2].pos

  -- Moller Trumbore transcription from Wikipedia (comments added)
  var e1  = p1 - p0
  var e2  = p2 - p0

  var h   = G.cross(r1-r0, e2)
  -- a  is the double-pyramid volume formed by the line segment and triangle
  var a   = G.dot(e1, h)          -- a = [r1-r0;e2;e1]
  -- reject any line segment parallel to the triangle
  if a > -EPS and a < EPS then return false end
  var f   = 1.0f / a
  var s   = r0 - p0
  -- u  is the coordinate on the e1 basis of the triangle
  var u   = f * G.dot(s, h)       -- u = [r1-r0;e2;r0-p0] / a
  if u < 0.0f or u > 1.0f then return false end
  var q   = G.cross(s, e1)
  -- v  is the coordinate on the e2 basis of the triangle
  var v   = f * G.dot(r1-r0, q)   -- v = [r1-r0;r0-p0;e1] / a
  if v < 0.0f or u + v > 1.0f then return false end
  var t   = f * G.dot(e2, q)      -- t = [r0-p0;e1;e2] / a
  if t > EPS and t < 1.0f-EPS then return true
                              else return false end
end


local gong join find_et_iscts ()
  e <- ShadowRays
  t <- Tris
  where ray_tri(r,t)
do
  emit { edge=e, tri=t, r=r } in ETcontacts
end
