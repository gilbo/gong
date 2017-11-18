import 'gong'

local Exports = {}
package.loaded['grid'] = Exports

local AABB    = require 'aabb'
local AABB3f  = AABB.AABB3f


local gong partition Grid3f(n) {
  n       : G.size32
  dims    : G.vec3u
  widths  : G.vec3f
  origin  : G.vec3f

  init( ds : G.vec3u, ws : G.vec3f, o : G.vec3f )
    self.n      = ds[0]*ds[1]*ds[2]
    self.dims   = ds
    self.widths = ws
    self.origin = o
  end

  -- assuming that the box is guaranteed to
  -- intersect some part of the grid
  generate( bb : AABB3f )
    var d     = self.dims
    var lo_v  = (bb.lo - self.origin)
    var hi_v  = (bb.hi - self.origin)
    var lo    : G.vec3i
    var hi    : G.vec3i
    for k=0,3 do
      lo[k]   = G.clamp( G.floor(lo_v[k] / self.widths[k] ), 0, d[k]-1 )
      hi[k]   = G.clamp( G.ceil( hi_v[k] / self.widths[k] ), 0, d[k]-1 )
    end

    for xi=lo[0],hi[0] do   for yi=lo[1],hi[1] do   for zi=lo[2],hi[2] do
      var id = xi + d[0]*yi + d[0]*d[1]*zi
      emit(id)
    end end end
  end
}
exports.Grid3f = Grid3f


local gong partition Grid2f(n) {
  n       : G.size32
  dims    : G.vec2u
  widths  : G.vec2f
  origin  : G.vec2f

  init( ds : G.vec2u, ws : G.vec2f, o : G.vec2f )
    self.n      = ds[0]*ds[1]
    self.dims   = ds
    self.widths = ws
    self.origin = o
  end

  -- assuming that the box is guaranteed to
  -- intersect some part of the grid
  generate( bb : AABB3f )
    var d     = self.dims
    var lo_v  = (bb.lo - self.origin)
    var hi_v  = (bb.hi - self.origin)
    var lo    : G.vec2i
    var hi    : G.vec2i
    for k=0,2 do
      lo[k]   = G.clamp( G.floor(lo_v[k] / self.widths[k] ), 0, d[k]-1 )
      hi[k]   = G.clamp( G.ceil( hi_v[k] / self.widths[k] ), 0, d[k]-1 )
    end

    for xi=lo[0],hi[0] do   for yi=lo[1],hi[1] do
      var id = xi + d[0]*yi
      emit(id)
    end end
  end
}
exports.Grid2f = Grid2f



