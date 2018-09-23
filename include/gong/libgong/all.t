
import 'gong'
local G = gong.stdlib


local Exports = {}
package.loaded["gong.libgong.all"] = Exports



-------------------------------------------------------------------------------
-- Axis-Aligned Bounding-Box volumes
-------------------------------------------------------------------------------


local AABB2f = G.record {
  { 'lo', G.vec2f },
  { 'hi', G.vec2f },
}
local AABB3f = G.record {
  { 'lo', G.vec3f },
  { 'hi', G.vec3f },
}

local gong function AABB2f_isct( a : AABB2f, b : AABB2f ) : G.bool
  return not ( a.lo[0] > b.hi[0] or a.lo[1] > b.hi[1] or
               a.hi[0] < b.lo[0] or a.hi[1] < b.lo[1] )
end
local gong function AABB3f_isct( a : AABB3f, b : AABB3f ) : G.bool
  return not ( a.lo[0] > b.hi[0] or a.lo[1] > b.hi[1] or a.lo[2] > b.hi[2] or
               a.hi[0] < b.lo[0] or a.hi[1] < b.lo[1] or a.hi[2] < b.lo[2] )
end

local gong function AABB2f_union( a : AABB2f, b : AABB2f ) : AABB2f
  return { lo = :[i] G.min(a.lo[i], b.lo[i]),
           hi = :[i] G.max(a.hi[i], b.hi[i]) }
end
local gong function AABB3f_union( a : AABB3f, b : AABB3f ) : AABB3f
  return { lo = :[i] G.min(a.lo[i], b.lo[i]),
           hi = :[i] G.max(a.hi[i], b.hi[i]) }
end

local gong function AABB2f_midpoint( a : AABB2f ) : G.vec2f
  return (a.lo + a.hi)/2
end
local gong function AABB3f_midpoint( a : AABB3f ) : G.vec3f
  return (a.lo + a.hi)/2
end

Exports.AABB3f                  = AABB3f
Exports.AABB3f_isct             = AABB3f_isct
Exports.AABB3f_union            = AABB3f_union
Exports.AABB3f_midpoint         = AABB3f_midpoint

Exports.AABB2f                  = AABB2f
Exports.AABB2f_isct             = AABB2f_isct
Exports.AABB2f_union            = AABB2f_union
Exports.AABB2f_midpoint         = AABB2f_midpoint


-------------------------------------------------------------------------------
-- Discrete-Oriented-Polytope volumes
-------------------------------------------------------------------------------

local DOP7f = G.record {
  -- in the direction of   x y z
  { 'lo_axis', G.vec3f },
  { 'hi_axis', G.vec3f },
  -- in the direction of   +x+y+z -x+y+z +x-y+z +x+y-z
  --    given as sqrt(3) length axis vectors
  { 'lo_diag', G.vec4f },
  { 'hi_diag', G.vec4f },
}


local gong function DOP7f_isct( a : DOP7f, b : DOP7f ) : G.bool
  return not (
    a.lo_axis[0] > b.hi_axis[0] or a.lo_axis[1] > b.hi_axis[1] or
                                   a.lo_axis[2] > b.hi_axis[2] or
    a.hi_axis[0] < b.lo_axis[0] or a.hi_axis[1] < b.lo_axis[1] or
                                   a.hi_axis[2] < b.lo_axis[2] or
    a.lo_diag[0] > b.hi_diag[0] or a.lo_diag[1] > b.hi_diag[1] or
    a.lo_diag[2] > b.hi_diag[2] or a.lo_diag[3] > b.hi_diag[3] or
    a.hi_diag[0] < b.lo_diag[0] or a.hi_diag[1] < b.lo_diag[1] or
    a.hi_diag[2] < b.lo_diag[2] or a.hi_diag[3] < b.lo_diag[3]
  )
end

local gong function DOP7f_union( a : DOP7f, b : DOP7f ) : DOP7f
  return { lo_axis = :[i] G.min(a.lo_axis[i], b.lo_axis[i]),
           hi_axis = :[i] G.max(a.hi_axis[i], b.hi_axis[i]),
           lo_diag = :[i] G.min(a.lo_diag[i], b.lo_diag[i]),
           hi_diag = :[i] G.max(a.hi_diag[i], b.hi_diag[i]) }
end

local gong function DOP7f_midpoint( a : DOP7f ) : G.vec3f
  var pa = (a.lo_axis + a.hi_axis)/2
  var md = (a.lo_diag + a.hi_diag)/2
  var s  = (+[i] md[i])/4
  var v  = {s,s,s} - 2 * { md[1], md[2], md[3] }
  return (pa + v)/2
end


Exports.DOP7f                   = DOP7f
Exports.DOP7f_isct              = DOP7f_isct
Exports.DOP7f_union             = DOP7f_union
Exports.DOP7f_midpoint          = DOP7f_midpoint














