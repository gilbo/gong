import 'gong'

local G               = (gong.stdlib)

-- maybe turn this into molecular dynamics?


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
--local SHLib           = require 'spatial_hash'

local factor          = 2.001 -- 2 + epsilon ideally at a minimum
local cell_w          = G.Const(G.float, factor * sphere_radius:get())

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

  generate( s : Spheres )
    var inv_w = 1.0f / cell_w
    var sp    = inv_w * s.pos
    var r     = { sphere_radius, sphere_radius, sphere_radius }
    var min   = G.floor(  sp - r  )
    var max   = G.ceil(   sp + r  )
    for i=min[0],max[0] do
      for j=min[1],max[1] do
        for k=min[2],max[2] do
          emit(i,j,k)
    end end end
  end
}

local n_bins          = 1e4


local hashIndex       =  G.Index()
                          :Partition( 'bins', SpatialHash3f )
                          :List( 'list' )
                          (Spheres)


local gong build construct_hashing( xs : G.Set(Spheres) ) : hashIndex
  xs  <- Partition(xs, SpatialHash3f(n_bins))
  x   <- List(xs)
  return x
end


local gong traversal self_hash_traversal( a : hashIndex, b : hashIndex )
  ( a == b @ bins ) => { expand(a,b) } -- scan the bins
  ( a == b @ list ) => {
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











