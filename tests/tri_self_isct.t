import 'gong'
local test  = require 'tests.test'
local G = gong.stdlib

--local keyT            = G.uint32
local EPSILON         = 1.0e-7

local GPU_ON = not not terralib.cudacompile

------------------------------------------------------------------------------

local Verts           = G.NewTable('Verts')
  Verts:NewField( 'pos',      G.vec3f )
local Edges           = G.NewTable('Edges')
  Edges:NewField( 'hd',       Verts   )
  Edges:NewField( 'tl',       Verts   )
local Tris            = G.NewTable('Tris')
  Tris:NewField(  'v',        G.vector(Verts, 3) )

local ETcontacts      = G.NewTable('ETcontacts')
ETcontacts:NewField( 'edge',  Edges   )
ETcontacts:NewField( 'tri',   Tris    )
ETcontacts:NewField( 'pos',   G.vec3f )

if GPU_ON then
  ETcontacts:setGPUSizeLinear(1, Edges, 1, Tris, 10)
end

------------------------------------------------------------------------------

local cross = G.Macro(function(a,b)
  local typ = a:gettype()
  return gong quote
    var aa = a
    var bb = b
  in {
    aa[1] * bb[2] - aa[2] * bb[1],
   -aa[0] * bb[2] + aa[2] * bb[0],
    aa[0] * bb[1] - aa[1] * bb[0]
  } end
end)
local dot = G.Macro(function(a,b)
  return gong` +[i] a[i] * b[i]
end)

local gong function et_is_isct( e : Edges, t : Tris ) : { G.bool, G.vec3f }
  var v0  = t.v[0].pos
  var v1  = t.v[1].pos
  var v2  = t.v[2].pos
  var ee  = e.hd.pos - e.tl.pos
  var eo  = e.tl.pos

  var e1  = v1 - v0
  var e2  = v2 - v0

  var h   = cross(ee, e2)
  var a   = dot(e1, h)
  if a > -EPSILON and a < EPSILON then    return false, {0.f,0.f,0.f}   end
  var f   = 1.0f/a
  var s   = eo - v0
  var u   = f * dot(s,h)
  if u < 0.0f or u > 1.0f then            return false, {0.f,0.f,0.f}   end
  var q   = cross(s, e1)
  var v   = f * dot(ee, q)
  if v < 0.0f or u + v > 1.0f then        return false, {0.f,0.f,0.f}   end
  var t   = f * dot(e2, q)
  if t <= EPSILON or t >= 1.0f - EPSILON then return false, {0.f,0.f,0.f} end
  -- otherwise
  var pos = (1.0f-t)*e.tl.pos + t*e.hd.pos
  return true, pos
end

local gong join find_et_iscts( e : Edges, t : Tris )
  var pass,pos    = et_is_isct(e,t)
  where pass
do
  emit { edge=e, tri=t, pos=pos } in ETcontacts
end


------------------------------------------------------------------------------


local API = G.CompileLibrary {
  tables        = {},
  joins         = {find_et_iscts},
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
local ERR = macro(function(err)
  return quote
    C.printf("ERROR: %s\n", err)
    return 1
  end
end)
local IN_EPS = macro(function(x,y)
  return `(C.fabs(x-y) < 1.0e-7)
end)

local Tvec3f  = G.vec3f:terratype()
local Tv3     = G.vector(Verts,3):terratype()

local function gen_exec(for_gpu)
  local terra exec()
    var store       = API.NewStore()
    var Verts       = store:Verts()
    var Edges       = store:Edges()
    var Tris        = store:Tris()
    var ETcontacts  = store:ETcontacts()

    Verts:beginload(6)
    Verts:loadrow([Tvec3f]( {arrayof(float,1,0,0)}  ))
    Verts:loadrow([Tvec3f]( {arrayof(float,0,1,0)}  ))
    Verts:loadrow([Tvec3f]( {arrayof(float,0,0,1)}  ))
    Verts:loadrow([Tvec3f]( {arrayof(float,0,0,0)}  ))
    Verts:loadrow([Tvec3f]( {arrayof(float,1,1,1)}  ))
    Verts:loadrow([Tvec3f]( {arrayof(float,1,1,-1)} ))
    Verts:endload()
    CHECK_ERR(store)

    Edges:beginload(6)
    Edges:loadrow(0,1)
    Edges:loadrow(1,2)
    Edges:loadrow(2,0)
    Edges:loadrow(3,4)
    Edges:loadrow(4,5)
    Edges:loadrow(5,3)
    Edges:endload()
    CHECK_ERR(store)

    Tris:beginload(2)
    Tris:loadrow([Tv3]( {arrayof(uint, 0,1,2)} ))
    Tris:loadrow([Tv3]( {arrayof(uint, 3,4,5)} ))
    Tris:endload()
    CHECK_ERR(store)

    escape if for_gpu then emit quote
      store:find_et_iscts_GPU()
    end else emit quote
      store:find_et_iscts()
    end end end
    CHECK_ERR(store)

    var n_contact   = ETcontacts:getsize()
    C.printf("got %d contacts\n", n_contact)
    if n_contact ~= 2 then
      store:destroy()
      ERR("expected 2 contacts; got another number")
    end

    var t, e, p   = ETcontacts:tri():read_lock(),
                    ETcontacts:edge():read_lock(),
                    ETcontacts:pos():read_lock()
    var t0, t1    = t[0], t[1]
    var e0, e1    = e[0], e[1]
    var p0, p1    = p[0], p[1]
    ETcontacts:tri():read_unlock()
    ETcontacts:edge():read_unlock()
    ETcontacts:pos():read_unlock()

    C.printf("contact 0 (edge,tri,pos): %d %d %f %f %f\n", e0,t0,
                  p0.d[0], p0.d[1], p0.d[2] )
    C.printf("contact 1 (edge,tri,pos): %d %d %f %f %f\n", e1,t1,
                  p1.d[0], p1.d[1], p1.d[2] )
    if t0 == 1 then
      t0, t1 = t1, t0
      e0, e1 = e1, e0
      p0, p1 = p1, p0
    end
    if t0 ~= 0 or e0 ~= 3 then
      ERR("expected an intersection between tri 0 & edge 3")
    elseif not IN_EPS(p0.d[0], 1.0/3.0) or
           not IN_EPS(p0.d[1], 1.0/3.0) or
           not IN_EPS(p0.d[2], 1.0/3.0)
    then
      ERR("expected intersection 2 at (0.3333,0.3333,0.3333)")
    end
    if t1 ~= 1 or e1 ~= 0 then
      ERR("expeced an intersection between tri 1 & edge 2")
    elseif p1.d[0] ~= 0.5 or p1.d[1] ~= 0.5 or p1.d[2] ~= 0.0 then
      ERR("expected intersection 2 at (0.5,0.5,0.0)")
    end

    store:destroy()
    return 0
  end
  return exec
end

test.eq(gen_exec(false)(), 0)
if GPU_ON then
  test.eq(gen_exec(true)(), 0)
end

------------------------------------------------------------------------------

------------------------------------------------------------------------------
