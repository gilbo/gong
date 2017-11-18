import 'gong'

local G               = (gong.stdlib)

------------------------------------------------------------------------------
-- Declarative Specification absent loading data

-- Thesis about LeanMD: http://charm.cs.illinois.edu/newPapers/04-11/paper.pdf

--[[ TODO:

Knowing whether a pair of atoms form
a bond is not a trivial operation. To do this efficiently, each cell-pair object caches the
bond information. This cache needs to be updated once after every atom-migration step, as
between the atom-migration steps a cell-pair object always receives same atoms information
from the cells it interacts with. A hash-table with bond-index (a unique bond identifier)
as the key is used to cache the bond information. Using a hash-table makes the process
of updating the cache more efficient. As an additional optimization, a per-atom bond list
(list of bonds an atom is part of) is maintained on every processor to make cache updation
efficient
--]]

local Particles         = G.NewTable('Particles')
Particles:NewField('pos', G.vec3f)

-- Not sure why, but the Charm++ sample this is based on has a 
-- minimum interaction distance. See line 57 here:
-- https://charm.cs.illinois.edu/gerrit/gitweb?p=benchmarks/leanmd.git;a=blob;f=physics.h;hb=refs/heads/master
local min_radius_sq      = G.Const(G.float, 1.0)
local max_radius_sq      = G.Const(G.float, 26.0*26.0)

local PPContacts      = G.NewTable('PPContacts', {
                          join_policy = 'rebuild',
                          -- i.e. destroy and rebuild the table
                          -- every time a join emits into it.
                        })
PPContacts:NewField('s0', Particles)
PPContacts:NewField('s1', Particles)
PPContacts:NewField('d_sq',  G.float)

local gong join find_particle_iscts ()
  x <- Particles
  y <- Particles
  var r = y.pos - x.pos
  var d2  = G.dot(r,r)
  where d2 > min_radius_sq and d2 < max_radius_sq
do
  emit { s0=x, s1=y, d_sq=d2 } in PPContacts
end


------------------------------------------------------------------------------
-- Algorithmic / Functional Specfication

-- Grid approach
local GridLib           = require 'grid'

-- "k-away"
local k = 3
local epsilon = 0.00001
local cell_w          = sqrt(max_radius_sq:get()) / k + epsilon


local Grid     = GridLib.Grid3f(cell_w)

local gIndex         = Grid.Type



---------------------- TODO: finish example.

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
