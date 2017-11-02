import 'gong'

-- Trying to encode a simple photon map query from primary
-- hit locations. Using the hashgrid approach from:
-- https://dl.acm.org/citation.cfm?id=2448207

-- Could use the more complicated scheme from Ma and McCool
-- https://dl.acm.org/citation.cfm?id=569059
-- with multiple hash tables to do aKNN

-- Issues to be addressed:
-- 
-- kNN; Ideally would filter out all but closest k contacts
-- Also ideally, would return distance to kth contact or return them
-- sorted...


local G               = (gong.stdlib)

------------------------------------------------------------------------------
-- Declarative Specification absent loading data

local Photons         = G.NewTable('Photons')
Photons:NewField('pos', G.vec3f)
local QueryPoints     = G.NewTable('QueryPoints')
QueryPoints:NewField('pos', G.vec3f)
local max_radius      = G.Const(G.float, 4.0)


local max_neighbors = 50

local gong struct Neighbor {
  photon : Photons
  sqDistance : G.float
}
QueryPoints:NewField('neighbors', G.dynbuffer(Neighbor,max_neighbors))

local gong function get_distance_squared( q : QueryPoints, p : Photons ) : Bool
  var d   = q.pos - p.pos
  var d2  = G.dot(d,d)
  return d2
end

local gong join find_photons ()
  p <- Photons
  q <- QueryPoints
  d2 = get_distance_squared(q,p)
  where d2 < max_radius*max_radius
do
  q.neighbors multi_argmin(sqDistance)= {photon=p,sqDistance=d2}
end


------------------------------------------------------------------------------
-- Algorithmic / Functional Specfication

local factor          = 1.001 -- 1 + epsilon
local cell_w          = G.Const(G.float, factor * max_radius:get())

local gong partition SpatialHash3f[n] {
  n   : G.size32

  init( sz : G.size32 )
    n = sz
  end

  -- argument becomes key
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

  bin( p : Photons )
    var inv_w = 1.0f / cell_w
    var cell_p    = G.round(inv_w * p.pos)
    emit(cell_p[0],cell_p[1],cell_p[2])
  end
}

gong SpatialHash3f.generate( q : QueryPoints )
    var inv_w = 1.0f / cell_w
    var qp    = G.round(inv_w * q.pos)
    var r     = { max_radius, max_radius, max_radius }
    var min   = G.floor( qp - r )
    var max   = G.ceil( qp + r )
    for i=min[0],max[0] do
      for j=min[1],max[1] do
        for k=min[2],max[2] do
          emit(i,j,k)
    end end end
end

local n_bins          = 1e4


local hashIndex       =  G.Index()
                          :Partition( 'bins', SpatialHash3f )
                          :List( 'list' )
                          (Photons)

local QueryUnindexed  =  G.Unindexed(QueryPoints)

local gong build construct_hashing( xs : G.Set(Photons) ) : hashIndex
  xs  <- Partition(xs, SpatialHash3f(n_bins))
  x   <- List(xs)
  return x
end


local gong traversal hash_traversal( a : hashIndex, b : QueryUnindexed )
  ( a @ bins, b ) => { a.partition(b) } -- scan the bins
  ( a @ list, b ) => { expand(a) }
end


-- set the indexing option, build & traversal schemes
find_photon_queries:UseAlgorithm {
  index_left    = hashIndex,
  index_right   = QueryUnindexed,
  build_left    = construct_hashing,
  build_right   = nil, 
  traversal     = hash_traversal,
}


------------------------------------------------------------------------------
-- Imperative Specfication / Schedule


-- schedule for a single CPU

local hbSchedule = construct_hashing:Schedule()
  hbSchedule:QueueBefore('bins')   :Priority(0)
    :CPU(0)

local travSchedule = self_hash_traversal:Schedule()
  travSchedule:QueueBefore('bins')   :Priority(0)
    :CPU(0)

-- TODO: GPU schedule