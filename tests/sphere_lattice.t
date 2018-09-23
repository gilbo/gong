import 'gong'
local test  = require 'tests.test'
local G = gong.stdlib

--local keyT            = G.uint32
--local EPSILON         = 1.0e-7

local GPU_ON = not not terralib.cudacompile

------------------------------------------------------------------------------

local radius          = G.Global('radius', G.float, 1.0)
local Spheres         = G.NewTable('Spheres')
                          :NewField( 'pos',   G.vec3f )
local SSContacts      = G.NewTable('SSContacts')
                          :NewField( 'a',     Spheres )
                          :NewField( 'b',     Spheres )

if GPU_ON then
  SSContacts:setGPUSizeLinear(8, Spheres, 10)
end

------------------------------------------------------------------------------

local gong join sphere_self_isct ( a : Spheres, b : Spheres )
  where a ~= b
  var r2          = 4*radius*radius
  var d           = a.pos - b.pos
  where (+[i] d[i]*d[i]) <= r2
do
  emit { a=a, b=b } in SSContacts
end

------------------------------------------------------------------------------

local AABB3f = G.AABB3f

local gong function Spheres_to_AABB3f( s : Spheres ) : AABB3f
  var r = {radius, radius, radius}
  return { lo = s.pos-r, hi = s.pos+r }
end


local BVH             = G.bvh_index {
  table       = Spheres,
  volume      = AABB3f,
  abstract    = Spheres_to_AABB3f,
  vol_union   = G.AABB3f_union,
  point       = G.AABB3f_midpoint,
}

local BVH_Traversal   = G.bvh_bvh_traversal {
  left        = BVH,
  right       = BVH,
  vol_isct    = G.AABB3f_isct,
}

------------------------------------------------------------------------------

local API_Scan = G.CompileLibrary {
  tables        = {},
  joins         = {sphere_self_isct},
  terra_out     = true,
  gpu           = GPU_ON,
}

sphere_self_isct:set_cpu_traversal(BVH_Traversal)
local API_BVH = G.CompileLibrary {
  tables        = {},
  joins         = {sphere_self_isct},
  terra_out     = true,
  gpu           = GPU_ON,
}

local C   = terralib.includecstring [[
#include<stdio.h>
#include<math.h>
]]
local CHECK_ERR = macro(function(store)
  return quote if store:geterror() ~= nil then
    C.printf("%s\n", store:geterror())
    store:destroy()
    return 1
  end end
end)
local ERR = macro(function(err,...)
  err = "ERROR: "..err:asvalue().."\n"
  local args = {...}
  return quote
    C.printf(err, [args])
    return 1
  end
end)

local Tvec3f  = G.vec3f:terratype()
local Tkey    = G.row(Spheres):terratype()

local N       = 4
local function gen_exec(API, for_gpu)
  local terra exec()
    var store       = API.NewStore()
    var Spheres     = store:Spheres()
    var SSContacts  = store:SSContacts()

    var xcount : uint32[N*N*N]
    var ycount : uint32[N*N*N]
    var zcount : uint32[N*N*N]
    Spheres:beginload(N*N*N)
    for i=0,N do
      for j=0,N do
        for k=0,N do
          var id    = [Tkey](i*N*N + j*N + k)
          xcount[id] = 0
          ycount[id] = 0
          zcount[id] = 0
          var pos   = Tvec3f { d=arrayof(float,i*1.5f,j*1.5f,k*1.5f) }
          Spheres:loadrow(pos)
        end
      end
    end
    Spheres:endload()
    CHECK_ERR(store)

    escape if for_gpu then emit quote
      store:sphere_self_isct_GPU()
    end else emit quote
      store:sphere_self_isct()
    end end end
    CHECK_ERR(store)

    var n_expect    = 3*N*N*(N-1)
    var n_contact   = SSContacts:getsize()
    C.printf("got %d contacts\n", n_contact)
    if n_contact ~= n_expect then
      store:destroy()
      ERR("expected %d contacts, but found %d", n_expect, n_contact)
    end

    -- make sure each individual contact is valid
    -- and accumulate into counters
    var sa, sb, p = SSContacts:a():read_lock(),
                    SSContacts:b():read_lock(),
                    Spheres:pos():read_lock()
    for k=0,n_contact do
      var a, b    = sa[k], sb[k]
      var za, zb  = a%N, b%N
      var aa, bb  = a/N, b/N
      var ya, yb  = aa%N, bb%N
      var xa, xb  = aa/N, bb/N
      --C.printf("A B %d %d\n", a, b)
      --C.printf("  %d %d %d ; %d %d %d\n",xa,ya,za, xb,yb,zb)

      -- figure out which axis this intersection lies on...
      if     xa == xb and ya == yb and za+1 == zb then
        zcount[a] = zcount[a]+1
      elseif xa == xb and ya+1 == yb and za == zb then
        ycount[a] = ycount[a]+1
      elseif xa+1 == xb and ya == yb and za == zb then
        xcount[a] = xcount[a]+1
      else
        ERR(["found a bad intersection between "..
             "spheres at %d(%d,%d,%d) and %d(%d,%d,%d)"],
             a, xa,ya,za, b, xb,yb,zb)
      end
    end
    SSContacts:a():read_unlock()
    SSContacts:b():read_unlock()
    Spheres:pos():read_unlock()

    -- check the counters for the right values...
    for i=0,N do
      for j=0,N do
        for k=0,N do
          var id    = [Tkey](i*N*N + j*N + k)

          if i < N-1 and xcount[id] ~= 1 then
            ERR("expected 1 xcount at (%d,%d,%d)", i,j,k)
          elseif i == N and xcount[id] ~= 0 then
            ERR("expected 0 xcount at (%d,%d,%d)", i,j,k)
          end

          if j < N-1 and ycount[id] ~= 1 then
            ERR("expected 1 ycount at (%d,%d,%d)", i,j,k)
          elseif j == N and ycount[id] ~= 0 then
            ERR("expected 0 ycount at (%d,%d,%d)", i,j,k)
          end

          if k < N-1 and zcount[id] ~= 1 then
            ERR("expected 1 zcount at (%d,%d,%d)", i,j,k)
          elseif k == N and zcount[id] ~= 0 then
            ERR("expected 0 zcount at (%d,%d,%d)", i,j,k)
          end
        end
      end
    end

    store:destroy()
    return 0
  end
  return exec
end

test.eq(gen_exec(API_Scan, false)(), 0)
test.eq(gen_exec(API_BVH, false)(), 0)
if GPU_ON then
  test.eq(gen_exec(API_Scan, true)(), 0)
end

------------------------------------------------------------------------------

------------------------------------------------------------------------------







