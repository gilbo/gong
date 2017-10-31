import 'gong'

-- Trying to encode a simple photon map query from primary
-- hit locations. Using the hashgrid approach from:
-- https://dl.acm.org/citation.cfm?id=2448207

-- Could use the more complicated scheme from Ma and McCool
-- https://dl.acm.org/citation.cfm?id=569059
-- with multiple hash tables to do aKNN

-- Issues to be addressed:
--  Trivial index with hash: No need to put query points
-- in hash grid, but how to query hashgrid then...
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

local QPContacts      = G.NewTable('QPContacts', {
                          join_policy = 'rebuild',
                          -- i.e. destroy and rebuild the table
                          -- every time a join emits into it.
                        })
QPContacts:NewField('p', Photons)
QPContacts:NewField('q', QueryPoints)
QPContacts:NewField('d2',  G.float)

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
  emit { p=p, q=q, d2=d2 } in QPContacts
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

  generate( p : Photons )
    var inv_w = 1.0f / cell_w
    var cell_p    = G.round(inv_w * p.pos)
    emit(cell_p[0],cell_p[1],cell_p[2])
  end
}

local n_bins          = 1e4


local hashIndex       =  G.Index()
                          :Partition( 'bins', SpatialHash3f )
                          :List( 'list' )
                          (Photons)
--[[
local trivialIndex    =  G.Index()
                          :List( 'list' )
                          (QueryPoints)
--]]

local gong build construct_hashing( xs : G.Set(Photons) ) : hashIndex
  xs  <- Partition(xs, SpatialHash3f(n_bins))
  x   <- List(xs)
  return x
end


local gong traversal hash_traversal( a : hashIndex, b : hashIndex )
  ( a @ bins, b @ list ) => { expand(a) } -- scan the bins
  ( a @ list, b @ list ) => {
    expand(a,b)           -- cross product of bins
  }
end


-- set the indexing option, build & traversal schemes
find_photon_queries:UseAlgorithm {
  index_left    = hashIndex,
  index_right   = hashIndex, -- This is wrong, make no acceleration structure over query points
  build_left    = construct_hashing,
  build_right   = construct_hashing, -- This is wrong, make no acceleration structure over query points
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