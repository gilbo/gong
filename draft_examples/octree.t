import 'gong'

local Exports = {}
package.loaded['octree'] = Exports


local AABB    = require 'aabb'
local AABB3f  = AABB.AABB3f


function Exports.Octree(params)

  local gong partition OctSplit(8) {
    derived   level     : G.uint
    derived   center    : G.vec3f
    derived   halfwidth : G.float

    derive_from()
      self.level      = 0
      self.center     = params.center
      self.halfwidth  = params.halfwidth
    end

    derive_from( p : OctSplit, id : G.uint32 )
      var hw          = p.halfwidth / 2.0f
      var c           = p.center
      c[0] += ( id%2    == 0)? -hw : hw
      c[1] += ((id/2)%2 == 0)? -hw : hw
      c[2] += ((id/4)%2 == 0)? -hw : hw

      self.level      = p.level + 1
      self.center     = c
      self.halfwidth  = hw
    end

    -- assuming that the box is guaranteed to
    -- intersect some part of the grid
    mask( bb : AABB3f ) : G.bits(8)
      var hw  = self.halfwidth
      var c   = self.center

      var x_bits =    ( (bb.lo[0] < c)? 1 : 0 )
                 + 2 *( (bb.hi[0] > c)? 1 : 0 )
      var y_bits =    ( (bb.lo[1] < c)? 1 : 0 ) * x_bits
                 + 4 *( (bb.hi[1] > c)? 1 : 0 ) * x_bits
      var z_bits =    ( (bb.lo[2] < c)? 1 : 0 ) * y_bits
                 + 16*( (bb.hi[2] > c)? 1 : 0 ) * y_bits
      return z_bits
    end
  }

  local OctTreeType   =  G.Index()
                          :Rec('oct_node', G.Index()
                            :Partition('split', OctSplit)
                          )

  return {
    OctSplit        = OctSplit,
    Type            = OctTreeType,
  }
end



