
local Exports = {}
package.loaded['octree'] = Exports



local gong partition OctSplit(8) {
  init( a : G.uint, c : G.float )
    self.axis   = a
    self.coord  = c
  end

  -- blah
  mask( bb : AABB3f ) : G.bits(2)
    var a   = G.uint(self.axis)
    var lo  = (bb.lo[a] <= self.coord)? 1 : 0
    var hi  = (bb.hi[a] >= self.coord)? 1 : 0
    return 2*hi + lo
  end
}

function Exports.PartialKDTree()
  local KD_Template   =  G.Index()
                          :Rec('kd_node', G.Index()
                            :Partition('split', axisSplit)
                          )

  local gong function is_split( bb : AABB3f, axis : G.uint, coord : G.float )
    return bb.lo[axis] <= coord and bb.hi[axis] >= coord then
  end
  -- blah
  local function gen_mid_build(name, params)
    local index     = assert(params.index,    'expected named arg: index')
    local midfunc   = assert(params.midfunc,  'expected named arg: midfunc')
    local cutoff    = assert(params.cutoff,   'expected named arg: cutoff')
    --local imbalance = assert(params.imbalance,
    --                         'expected named arg: imbalance')
    local sub_build = assert(params.sub_build,
                             'expected named arg: sub_build')
    local set       = index:BaseSet()

    local gong build build_bvh( xs : set ) : index
      var axis    = 0
      var overlap = 0
      var mid     = 0.0f
      do
        var x3    = Sample(xs, 3)
        mid       = mid_of_3( midfunc(x3[0])[axis],
                              midfunc(x3[1])[axis],
                              midfunc(x3[2])[axis] )
        overlap   = reduce(+)(xs, (x) => { is_split(x, axis, mid)? 1 : 0 })
      end

      while #xs > cutoff and overlap < #xs/2do
        axis      = (axis+1)%n_dims
        xs <- Split(xs, 2,  ( x : set ) => {
                              return (midfunc(x)[axis] < mid)? 0 : 1
                            })
        var x3    = Sample(xs, 3)
        mid       = mid_of_3( midfunc(x3[0])[axis],
                              midfunc(x3[1])[axis],
                              midfunc(x3[2])[axis] )
        overlap   = reduce(+)(xs, (x) => { is_split(x, axis, mid)? 1 : 0 })
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

function Exports.FullKDTree(n_leaf)
  local KD      = Exports.PartialKDTree()
  local SubType = G.Index()
                    :List('leaf_list')
  local Type    = KD.Type(SubType)

  local function gen_mid_build(name, params)
    local index     = assert(params.index, 'expected named arg: index')
    local midfunc   = assert(params.midfunc, 'expected named arg: midfunc')
    local set       = index:BaseSet()

    local gong
    build sub_build_fullkd( xs : G.Set(set) ) : SubType(set)
      x <- List(xs)
    end

    local params2 = {}
    for k,v in pairs(params) do params2[k] = v end
    params2.sub_build = sub_build_fullkd
    params2.cutoff    = n_leaf

    return BVH.gen_mid_build(name, params2)
  end

  return {
    Type            = Type,
    gen_mid_build   = gen_mid_build,
  }
end
