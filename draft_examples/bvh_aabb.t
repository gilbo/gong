import 'gong'

local Exports = {}
package.loaded['bvh_aabb'] = Exports

local gong abstraction AABB3f {
  lo : G.vec3f
  hi : G.vec3f

  abstractall( as : G.Set(AABB3f) )
    for a in as do
      lo min= a.lo
      hi max= a.hi
    end
  end

  intersect( a : AABB3f, b : AABB3f )
    lo = G.max(a.lo, b.lo)
    hi = G.min(a.hi, b.hi)
  end

  check( a : AABB3f )
    return a.lo[0] < a.hi[0] and a.lo[1] < a.hi[1] and a.lo[2] < a.hi[2]
  end
  check( a : AABB3f, b : AABB3f )
    return (a.hi[0] > b.lo[0] and a.lo[0] < b.hi[0])
       and (a.hi[1] > b.lo[1] and a.lo[1] < b.hi[1])
       and (a.hi[2] > b.lo[2] and a.lo[2] < b.hi[2])
  end
}
Exports.AABB3f = AABB3f

local gong
function mid_of_3( a : G.float, b : G.float, c : G.float ) : G.float
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

function Exports.PartialBVH(aabb, n_dims)
  local BVH_Template  =  G.Index()
                          :Rec('bvh_node', G.Index()
                                :Abstract('box',    aabb)
                                :Split('children',  2)
                          )

  local function gen_mid_build(name, params)
    local index     = assert(params.index, 'expected named arg: index')
    local midfunc   = assert(params.midfunc, 'expected named arg: midfunc')
    local cutoff    = assert(params.cutoff, 'expected named arg: cutoff')
    local sub_build = assert(params.sub_build,
                             'expected named arg: sub_build')
    local set       = index:BaseSet()

    local gong build build_bvh( xs : set ) : index
      var axis    = 0
      while #xs > cutoff do
        abstract(xs, aabb)
        var x3    = Sample(xs, 3)
        var mid   = mid_of_3( midfunc(x3[0])[axis],
                              midfunc(x3[1])[axis],
                              midfunc(x3[2])[axis] )
        xs <- Split(xs, 2,  ( x : set ) => {
                              return (midfunc(x)[axis] < mid)? 0 : 1
                            })
        axis      = (axis+1)%n_dims
      end
      sub_build(xs)
    end
    build_bvh:setname(name)
    return build_bvh
  end

  return {
    Type            = BVH_Template,
    gen_mid_build   = gen_mid_build,
  }
end

function Exports.FullBVH(aabb, n_dims, n_leaf)
  local BVH     = Exports.PartialBVH(aabb, n_dims)
  local SubType = G.Index()
                    :Abstract('leaf_box', AABB3f)
                    :List('leaf_list',  { max=n_leaf })
                    :Abstract('item_box', AABB3f)
  local Type    = BVH.Type(SubType)

  local function gen_mid_build(name, params)
    local index     = assert(params.index, 'expected named arg: index')
    local midfunc   = assert(params.midfunc, 'expected named arg: midfunc')
    local set       = index:BaseSet()

    local gong
    build sub_build_fullbvh( xs : set ) : SubType(set)
      abstract(xs, aabb)
      x <- List(xs)
      abstract(x, aabb)
    end

    local params2 = {}
    for k,v in pairs(params) do params2[k] = v end
    params2.sub_build = sub_build_fullbvh
    params2.cutoff    = n_leaf

    return BVH.gen_mid_build(name, params2)
  end

  return {
    Type            = Type,
    gen_mid_build   = gen_mid_build,
  }
end

