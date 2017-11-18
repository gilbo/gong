import 'gong'

local G               = (gong.stdlib)

------------------------------------------------------------------------------
-- Declarative Specification absent loading data

local Spheres         = G.NewTable('Spheres')
Spheres:NewField('pos', G.vec3f)
local sphere_radius   = G.Const(G.float, 1.0)

local SSContacts      = G.NewTable('SSContacts', {
                          join_policy = 'rebuild',
                          -- i.e. destroy and rebuild the table
                          -- every time a join emits into it.
                        })
SSContacts:NewField('s0', Spheres)
SSContacts:NewField('s1', Spheres)
SSContacts:NewField('r',  G.vec3f)

local gong function is_isct_spheres( x : Spheres, y : Spheres ) : Bool
  var d   = y.pos - x.pos
  var d2  = G.dot(d,d)
  return d2 < sphere_radius * sphere_radius
end

local gong join find_sphere_iscts ()
  x <- Spheres
  y <- Spheres
  var r = y.pos - x.pos
  where is_isct_spheres(x,y)
do
  emit { s0=x, s1=y, r=r } in SSContacts
end


------------------------------------------------------------------------------
-- Algorithmic / Functional Specfication

-- Grid approach
local SHLib           = require 'spatial_hash'

local factor          = 2.001 -- 2 + epsilon ideally at a minimum
local cell_w          = factor * sphere_radius:get()

local hash_factor     = 2

local SpatialHash     = SHLib.SpatialHash3f(cell_w, hash_factor)

local hashIndex         = SpatialHash.Type
local construct_hashing = SpatialHash.gen_build('construct_hashing',
                                                hashIndex)

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


------------------------------------------------------------------------------
-- Imperative Specfication / Schedule


-- schedule for a single CPU

local hbSchedule = construct_hashing:Schedule()
  hbSchedule:QueueBefore('bins')   :Priority(0)
    :CPU(0)
  -- allow the listing to be fused in

local travSchedule = self_hash_traversal:Schedule()
  travSchedule:QueueBefore('bins , bins')   :Priority(0)
    :CPU(0)











