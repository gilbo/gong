import 'gong'

local G               = (gong.stdlib)


------------------------------------------------------------------------------
-- Declarative Specification absent loading data

local keyT            = G.uint32

local Verts           = G.NewTable('Verts')
local Edges           = G.NewTable('Edges')
local Tris            = G.NewTable('Tris')

Verts:NewField('pos', G.vec3f)
Edges:NewField('hd', Verts, { keyrep = keyT })
Edges:NewField('tl', Verts, { keyrep = keyT })
Tris:NewField('v',   G.vector(Verts, 3), { keyrep = keyT })

local ETcontacts      = G.NewTable('ETcontacts', {
                          join_policy = 'rebuild',
                          -- i.e. destroy and rebuild the table
                          -- every time a join emits into it.
                        })
ETcontacts:NewField('edge', Edges)
ETcontacts:NewField('tri',  Tris)
ETcontacts:NewField('pos',  G.vec3f)

local gong function et_is_isct( e : Edges, t : Tris ) : { Bool, G.vec3f }
  -- math
end

local gong join find_et_iscts ()
  e <- Edges
  t <- Tris
  var pass,pos = et_is_isct
  where pass
do
  emit { edge=e, tri=t, r=r } in ETcontacts
end


------------------------------------------------------------------------------
-- Algorithmic / Functional Specfication

local n_leaf          = 8

local AABB3f          = BVHlib.AABB3f
local BVH             = BVHlib.FullBVH(AABB3f, 3, n_leaf)

gong AABB3f.abstract( e : Edges )
  lo = G.min(e.hd.pos, e.tl.pos)
  hi = G.max(e.hd.pos, e.tl.pos)
end
gong AABB3f.abstract( t : Tris )
  lo = G.min( t.v[0].pos, G.min( t.v[1].pos, t.v[2].pos ))
  hi = G.max( t.v[0].pos, G.max( t.v[1].pos, t.v[2].pos ))
end

local EdgeIndex       = BVH.Type(Edges)
local TriIndex        = BVH.Type(Tris)

local gong function eMid( e : Edges ) : G.vec3f
  return 0.5f * (e.hd.pos + e.tl.pos)
end
local gong function tMid( t : Tris ) : G.vec3f
  return (1.0f/3.0f) * (t.v[0].pos + t.v[1].pos + t.v[2].pos)
end

local build_PlankIndex = BVH.gen_mid_build('build_PlankIndex', {
  index     = PlankIndex,
  midfunc   = pMid,
})










local gong function mid_of_3( a : G.float, b : G.float, c : G.float ) : G.float
  var lo : G.float  = 0.0f
  var hi : G.float  = 0.0f
  if a > b then hi = a ; lo = b
           else hi = b ; lo = a end
  var mid : G.float = 0.0f
  if      c < lo then mid = lo
  elseif  c > hi then mid = hi
                 else mid = c  end
  return mid
end

local function build_gen(name, set, index, midfunc)
  local gong build build_bvh( xs : G.Set(set) ) : index
    while #xs > n_leaf do
      abstract(xs, AABB3f)
      var x3    = Sample(xs, 3)
      var axis  = random(3)
      var mid   = mid_of_3( midfunc(x3[0])[axis],
                            midfunc(x3[1])[axis],
                            midfunc(x3[2])[axis] )
      xs <- Split(xs, 2,  ( x : set ) => {
                            return (midfunc(x)[axis] < mid)? 0 : 1
                          })
    end
    abstract(xs, AABB3f)
    x <- List(xs)
    abstract(x, AABB3f)
  end
  build_bvh:setname(name)
  return build_bvh
end

local build_EdgeIndex = build_gen('build_EdgeIndex', Edges, EdgeIndex, eMid)
local build_TriIndex  = build_gen('build_TriIndex', Tris, TriIndex, tMid)

local gong traversal et_bvh_traverse( e : EdgeIndex, t : TriIndex )
  ( e @ node.box, t @ node.box) => {
    check(e,t)
    var flip = random(2)
    if flip == 0 then   expand(e,e)
                 else   expand(t,t) end
  }
  ( e @ node.box, t @ leaf_box ) => { check(e,t); expand(e,e) }
  ( e @ leaf_box, t @ node.box ) => { check(e,t); expand(t,t) }
  ( e @ leaf_box, t @ leaf_box ) => { check(e,t); expand(e,t,e,t) }
  ( e @ item_box, t @ item_box ) => { check(e,t); expand(e,t) }
end

-- set the indexing option, build & traversal schemes
find_sphere_iscts:UseAlgorithm {
  index_left    = EdgeIndex,
  index_right   = TriIndex,
  build_left    = build_EdgeIndex,
  build_right   = build_TriIndex,
  traversal     = et_bvh_traverse,
}


------------------------------------------------------------------------------
-- Imperative Specfication / Schedule

-- schedule for a single CPU

local ebSchedule = build_EdgeIndex:Schedule()
  ebSchedule:QueueBefore('node.children')   :Priority(0)
    :LIFO() -- depth first
    :CPU(0)
  ebSchedule:QueueBefore('leaf_list')       :Priority(2)
    :CPU(0)


local tbSchedule = build_TriIndex:Schedule()
  tbSchedule:QueueBefore('node.children')
    :LIFO() -- depth first
    :CPU(0)
  tbSchedule:QueueBefore('leaf_list')       :Priority(2)
    :CPU(0)


local travSchedule = et_bvh_traverse:Schedule()
  travSchedule:QueueBefore('node.box , node.box')   :Priority(0)
    :LIFO()
    :CPU(0)
  travSchedule:QueueBefore('node.box , leaf_box')   :Priority(2)
    :LIFO()
    :CPU(0)
  travSchedule:QueueBefore('leaf_box , node.box')   :Priority(4)
    :LIFO()
    :CPU(0)
  travSchedule:QueueBefore('leaf_box , leaf_box')   :Priority(6)
    :LIFO()
    :CPU(0)
  travSchedule:QueueBefore('item_box , item_box')   :Priority(8)
    :CPU(0)





-- fusion data
--[[
ProgramPoint:QueueHere()
Queue:Inline( arg )

Queue:Batch(n)

Queue:Replicate{ k=#, ... }
  can do stealing or other load-balancing

Queue:CPU(k)
  {Queue}:CPU(k)
  {Queue}:CPU({k1,k2,...})

Queue:GPU(k)
  ...

Queue:LIFO()
Queue:Priority(k)

Queue:Parallelize(prog)
  prog is drawn from this language:
    DP  ::=   Seq(k).<DP>
          |   CPUs(k).<DP>
          |   GPUBlock(k).<DP>
          |   Vectorize(k).<DP>
          |   GPUWarp(k).<DP>
          |   END
--]]


