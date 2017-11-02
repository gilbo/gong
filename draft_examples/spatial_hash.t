import 'gong'

local Exports = {}
package.loaded['spatial_hash'] = Exports

local AABB    = require 'aabb'
local AABB3f  = AABB.AABB3f


local function Exports.SpatialHash3f(cell_w, hash_factor)

  local gong partition SpatialHash3f(n) {
    n : G.size32
    init( sz : G.size32 )
      n = sz
    end

    use hashing( i : int32, j : int32, k : int32 ) : uint32
      --var Xmult = 0x3a99068f
      --var Ymult = 0xbe93625f
      --var Zmult = 0xe823bd4f
      var Xmult   = [G.int32](73856093)
      var Ymult   = [G.int32](19349663)
      var Zmult   = [G.int32](83492791)
      var hid     = (i*Xmult) ^ (j*Ymult) ^ (k*Zmult)
      return hid % n
    end

    generate( a : AABB3f )
      var inv_w = 1.0f / cell_w
      var lo    = inv_w * a.lo
      var hi    = inv_w * a.hi
      for i=lo[0],hi[0] do
        for j=lo[1],hi[1] do
          for k=lo[2],hi[2] do
            emit(i,j,k)
      end end end
    end
  }

  local HashIndex   = G.Index()
                        :Partition( 'sh_bins', SpatialHash3f )
                        :List( 'sh_list' )

  local function gen_build(name, Index)
    local set       = Index:BaseSet()
    local gong build build_hash( xs : set ) : Index
      xs  <- Partition(xs, SpatialHash3f(#xs * hash_factor))
      x   <- List(xs)
      return x
    end
    build_hash:setname(name)
    return build_hash
  end

  return {
    Type            = HashIndex,
    gen_build       = gen_build,
  }
end





local gong traversal self_hash_traversal( a : hashIndex, b : hashIndex )
  ( a == b @ sh_bins ) => { expand(a,b) } -- scan the bins
  ( a == b @ sh_list ) => {
    expand(a,b)           -- cross product of bins
  }
end

self_hash_traversal:DedupFilterBeforeAction()


-- set the indexing option, build & traversal schemes
find_sphere_iscts:UseAlgorithm {
  index_left    = hashIndex,
  index_right   = hashIndex,
  build_left    = construct_hashing,
  build_right   = construct_hashing,
  traversal     = self_hash_traversal,
}

