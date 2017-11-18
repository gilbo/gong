import 'gong'

local Exports = {}
package.loaded['aabb'] = Exports

local gong abstraction AABB3f {
  lo : G.vec3f
  hi : G.vec3f

  abstractall( as : G.Set(AABB3f) )
    for a in as do
      lo min= a.lo
      hi max= a.hi
    end
  end

  check( a : AABB3f, b : AABB3f )
    return (a.hi[0] > b.lo[0] and a.lo[0] < b.hi[0])
       and (a.hi[1] > b.lo[1] and a.lo[1] < b.hi[1])
       and (a.hi[2] > b.lo[2] and a.lo[2] < b.hi[2])
  end
}
Exports.AABB3f = AABB3f

local gong abstraction AABB2f {
  lo : G.vec2f
  hi : G.vec2f

  abstractall( as : G.Set(AABB2f) )
    for a in as do
      lo min= a.lo
      hi max= a.hi
    end
  end

  check( a : AABB2f, b : AABB2f )
    return (a.hi[0] > b.lo[0] and a.lo[0] < b.hi[0])
       and (a.hi[1] > b.lo[1] and a.lo[1] < b.hi[1])
  end
}
Exports.AABB2f = AABB2f

