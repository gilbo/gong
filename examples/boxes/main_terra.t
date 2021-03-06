import 'gong'
local G = gong.stdlib

-- ensure that the directory of this script is on the $PATH
local Pathname  = (require "gong.tools.pathname").Pathname
local scriptdir = (Pathname.scriptdir()):abspath():cleanpath()
--local libdir    = (Pathname.ebb_root..'include/gong/tools/'):cleanpath()
package.terrapath = package.terrapath..';'..tostring(scriptdir)..'/?.t'

local Prelude   = require 'prelude'
local taketime  = Prelude.taketime

local PCG       = require 'PCG_terra'
local PGS       = require 'PGS_terra'

local newlist   = terralib.newlist

local vdb       = require 'gong.tools.vdb'

local C         = Prelude.C
local assert    = G.cassert

------------------------------------------------------------------------------
-- PRNG from https://burtleburtle.net/bob/rand/smallprng.html

local rot = macro(function(x,k)
  return quote
    var tmp0  = x << k
    var tmp1  = x >> (32-k)
  in
    tmp0 or tmp1
  end
end)

struct rand_ctxt { a : uint32, b : uint32, c : uint32, d : uint32 }
local RAND_CTXT = global(rand_ctxt)

terra randval() : uint32
  var x   = [&rand_ctxt](&RAND_CTXT)
  var e   = x.a - rot(x.b, 27)
  x.a     = x.b ^ rot(x.c, 17)
  x.b     = x.c + x.d
  x.c     = x.d + e
  x.d     = e + x.a
  return x.d
end

terra randinit( seed : uint32 )
  var x   = [&rand_ctxt](&RAND_CTXT)
  x.a = seed
  x.b = seed
  x.c = seed
  x.d = seed
  for i = 0,20 do
    randval()
  end
end

terra randdouble() : double
  var r : double = [double](randval())
  return r / double([ math.pow(2,32)-1 ])
end

------------------------------------------------------------------------------
-- Sim Constants / Parameters

local function boolparam(str)
  if type(str) == 'boolean' then return str end
  if str == nil or str == '' or str == '0' or str == 'false' then
    return false
  else
    return true
  end
end
local params = { debug_draw     = false,
                 traversal      = 'scan_scan',
                 collide_shape  = 'aabb',
                 no_projectile  = false,
                 effect_buffer  = false,
                 index_buffer   = false,
                 verify         = false,
                 test_case      = 'round_tower',
                 load_dir       = nil,
                 tower_levels   = 25,
                 tower_ring     = 24,
                 max_iters      = 50,
                 gpu            = false,
                 seed           = 0,
               }
for _,a in ipairs(arg) do
  local _, _, label, value = a:find("^%-([^=]*)=(.*)$")
  if label and value then params[label] = value end
end

local debug_draw            = boolparam(params.debug_draw)

local traversal             = params.traversal
local collide_shape         = params.collide_shape
local test_case             = params.test_case
local load_dir              = params.load_dir
local tower_levels          = tonumber(params.tower_levels)
local tower_ring            = tonumber(params.tower_ring)
local use_projectile        = not boolparam(params.no_projectile)
local effect_buffer         = boolparam(params.effect_buffer)
local index_buffer          = boolparam(params.index_buffer)
local verify                = boolparam(params.verify)
local use_gpu               = boolparam(params.gpu)

local timestep              = 1/60
local inv_timestep          = 1/timestep

local friction_coefficient  = 0.30
local gravity_acc           = 9.8

local mass                  = 1
local invmass               = 1/mass

local max_iters             = tonumber(params.max_iters)
local seed                  = tonumber(params.seed)
randinit(seed)

-- NOTE: The PCG solver doesn't actually work
--        I'm not sure it ever did
local solver_use            = 'pgs'

------------------------------------------------------------------------------
-- API Generation

local function GenBoxAPI()
  local Boxes             = require 'boxes'

  if traversal == 'bvh_bvh' then
    if collide_shape == 'aabb' then
      Boxes.find_plank_iscts:set_cpu_traversal(Boxes.aabb_bvh_traversal)
      Boxes.find_plank_iscts:set_gpu_traversal(Boxes.aabb_bvh_traversal)
    elseif collide_shape == 'dop' then
      Boxes.find_plank_iscts:set_cpu_traversal(Boxes.dop_bvh_traversal)
      Boxes.find_plank_iscts:set_gpu_traversal(Boxes.dop_bvh_traversal)
    else error('unrecognized collision shape: '..collide_shape)
    end
  elseif traversal == 'hash_split' then
    local cell_w = 3
    if test_case == 'slant_ground' then
    elseif test_case == 'simple_stack' then
    elseif test_case == 'round_tower' then
    elseif test_case == 'round_tower0' then
    elseif test_case == 'round_tower1' then
    elseif test_case == 'round_tower2' then
    elseif test_case == 'plank_tower1' then
      error("hash_split unsupported for '"..test_case.."'")
    elseif test_case == 'plank_tower2' then
      error("hash_split unsupported for '"..test_case.."'")
    else
      error('unrecognized test-case: '..tostring(test_case))
    end
    Boxes.find_plank_iscts:set_cpu_traversal(Boxes.hash_gen(cell_w*2.0))
  elseif traversal == 'scan_scan' then
    -- a-ok
  else
    error('unrecognized traversal setting: '..traversal)
  end

  if effect_buffer then
    Boxes.find_plank_iscts:buffer_effects_on_cpu()
  end
  if index_buffer then
    Boxes.find_plank_iscts:buffer_index_on_cpu()
  end
  Boxes.find_plank_iscts:verify_index(verify)

  if use_gpu then
    Boxes.PPContacts:setGPUSizeLinear(3,Boxes.Planks,64)
  end

  local API = G.CompileLibrary {
    tables            = Boxes.tables,
    joins             = Boxes.joins,
    terra_out         = true,
    gpu               = use_gpu,
  }

  local blockout = {
    tables            = true,
    joins             = true,
    Planks            = true,
    PPContacts        = true,
    find_plank_iscts  = true,
    bvh_traversal     = true,
  }
  for k,v in pairs(Boxes) do if not blockout[k] then API[k] = v end end

  return API
end

local API   = GenBoxAPI()
local vec3  = API.vec3:terratype()
local mat3  = API.mat3:terratype()
local quat  = API.quat:terratype()
local num   = API.num:terratype()

-- useful constructor function/macros
local v3 = macro(function(x,y,z)
  return `vec3{array(num(x),num(y),num(z))} end)
local m3 = macro(function(a00,a01,a02, a10,a11,a12, a20,a21,a22)
  return `mat3{array(num(a00),num(a10),num(a20),
                     num(a01),num(a11),num(a21),
                     num(a02),num(a12),num(a22))} end)
local q4 = macro(function(x,y,z,w)
  return `quat{array(num(x),num(y),num(z),num(w))} end)

Prelude.API_Extend(API)


------------------------------------------------------------------------------
-- Build box configuration

local terra loadContacts( store : API.Store )
  var contacts = store:PPContacts()
  contacts:beginload(0)
  contacts:endload()
end

local terra loadBoxesSlantGround( store : API.Store )
  var boxes = store:Planks()

  boxes:beginload( 2 )
    -- ground box
    var ang = 10.0 * [math.pi/180.0]
    --cos = 1 - 2qq
    --sin = 2qs
    --so,
    --q = sqrt( (1-cos)/2 )
    --s = sin / 2q
    var cos = C.cos(ang)
    var sin = C.sin(ang)
    var q   = C.sqrt( (1-cos)/2 )
    --var s   = sin / (2*q)
    -- s^2 = 1 - q^2 = 1 - (1-cos)/2 = (1+cos)/2
    var s   = C.sqrt( (1+cos)/2 )
    boxes:loadrow( v3(0,-50,0),     -- position
                   q4(q,0,0,s),     -- rotation
                   v3(0,0,0),       -- linear velocity
                   v3(0,0,0),       -- angular velocity
                   v3(0,0,0),       -- force
                   v3(0,0,0),       -- torque
                   num(0),          -- mass
                   v3(200,100,200)  -- dims
                 )
    boxes:loadrow( v3(0,3.0,0),
                   q4(0,0,0,1),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   num(1),
                   v3(2,1,2)
                 )

  boxes:endload()

  return 300
end

local terra loadBoxesSimpleStack( store : API.Store )
  var boxes = store:Planks()

  var N     = 4

  boxes:beginload( N + 1 )
    -- ground box
    boxes:loadrow( v3(0,-50,0),     -- position
                   q4(0,0,0,1),     -- rotation
                   v3(0,0,0),       -- linear velocity
                   v3(0,0,0),       -- angular velocity
                   v3(0,0,0),       -- position
                   v3(0,0,0),       -- position
                   num(0),          -- position
                   v3(200,100,200)  -- position
                 )
 -- boxes:loadrow( v3(0,0.5,0),
 --                q4(0,0,0,1),
 --                v3(0,0,0),
 --                v3(0,0,0),
 --                v3(0,0,0),
 --                v3(0,0,0),
 --                num(1),
 --                v3(2,1,2)
 --              )
 -- boxes:loadrow( v3(0,1.5,0),
 --                q4( 0, [math.sin(math.pi/8)], 0, [math.cos(math.pi/8)] ),
 --                v3(0,0,0),
 --                v3(0,0,0),
 --                v3(0,0,0),
 --                v3(0,0,0),
 --                num(1),
 --                v3(2,1,2)
 --              )
    -- simple boxes
    for k=0,N do
      var Y = 1.0*k + 0.5
      boxes:loadrow( v3(0,Y,0.6*k),
                     q4(0,0,0,1),
                     v3(0,0,0),
                     v3(0,0,0),
                     v3(0,0,0),
                     v3(0,0,0),
                     num(1),
                     v3(2,1,2)
                   )
    end
  boxes:endload()

  return 250
end

local terra loadBoxesRoundTower( store : API.Store )
  var boxes = store:Planks()

  var n_levels  = [ tower_levels ] -- 25, 50, 50
  var n_ring    = [ tower_ring ]   -- 24, 24, 48
  var radius    = [ 0.46 * tower_ring ] -- 11, 11, 22

  var n_extra   = 1
  if use_projectile then n_extra = 2 end

  boxes:beginload( n_levels*n_ring + n_extra )
    -- ground box
    boxes:loadrow( v3(0,-50,0),
                   q4(0,0,0,1),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   num(0),
                   v3(400,100,400)
                 )
  if use_projectile then
    -- projectile box
    var pos = v3(radius+10,radius*1.33,0)
    pos(0)  = pos(0) + (0.2 * randdouble() - 0.1)
    pos(1)  = pos(1) + radius * (0.02 * randdouble() - 0.01)
    C.printf("rand pos %f %f\n", pos(0), pos(1))
    boxes:loadrow( pos,   -- position
                   q4(0,0,0,1),   -- rotation
                   v3(-60,-30,0), -- linear velocity
                   v3(0,0,0),     -- angular velocity
                   v3(0,0,0),     -- force
                   v3(0,0,0),     -- torque
                   num(18),       -- mass
                   v3(2,7,5)      -- dims
                 )
  end
    for k=0,n_levels do
      var off = num((k+1)%2)/2
      for j=0,n_ring do
        var ang     = 2*math.pi/(n_ring) * (j+1+off)
        var rang    = -0.5*ang
        var x       = radius*C.cos(ang)
        var z       = radius*C.sin(ang)
        boxes:loadrow( v3(x,1.0*k+0.5,z),
                       q4(0,C.sin(rang),0,C.cos(rang)),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       num(1),
                       v3(2,1,2)
                     )
      end
    end
  boxes:endload()

  return 300
end

local terra loadBoxesRoundTower0( store : API.Store )
  var boxes = store:Planks()

  var n_levels  = 25
  var n_ring    = 24
  var radius    = 11

  var n_extra   = 1
  if use_projectile then n_extra = 2 end

  boxes:beginload( n_levels*n_ring + n_extra )
    -- ground box
    boxes:loadrow( v3(0,-50,0),
                   q4(0,0,0,1),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   num(0),
                   v3(200,100,200)
                 )
  if use_projectile then
    -- projectile box
    var pos = v3(20,14.65,0)
    pos(0)  = pos(0) - 0.01 + 0.02 * randdouble()
    pos(1)  = pos(1) - 0.01 + 0.02 * randdouble()
    C.printf("rand pos %f %f\n", pos(0), pos(1))
    boxes:loadrow( pos,   -- position
                   q4(0,0,0,1),   -- rotation
                   v3(-60,-30,0), -- linear velocity
                   v3(0,0,0),     -- angular velocity
                   v3(0,0,0),     -- force
                   v3(0,0,0),     -- torque
                   num(18),       -- mass
                   v3(2,7,5)      -- dims
                 )
  end
    for k=0,n_levels do
      var off = num((k+1)%2)/2
      for j=0,n_ring do
        var ang     = 2*math.pi/(n_ring) * (j+1+off)
        var rang    = 0.5*-ang
        var x       = radius*C.cos(ang)
        var z       = radius*C.sin(ang)
        boxes:loadrow( v3(x,1.0*k+0.5,z),
                       q4(0,C.sin(rang),0,C.cos(rang)),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       num(1),
                       v3(2,1,2)
                     )
      end
    end
  boxes:endload()

  return 300
end

local terra loadBoxesRoundTower1( store : API.Store )
  var boxes = store:Planks()

  var n_levels  = 50
  var n_ring    = 24
  var radius    = 11

  var n_extra   = 1
  if use_projectile then n_extra = 2 end

  boxes:beginload( n_levels*n_ring + n_extra )
    -- ground box
    boxes:loadrow( v3(0,-50,0),
                   q4(0,0,0,1),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   num(0),
                   v3(200,100,200)
                 )
  if use_projectile then
    -- projectile box
    boxes:loadrow( v3(20,14.65,0),   -- position
                   q4(0,0,0,1),   -- rotation
                   v3(-60,-30,0), -- linear velocity
                   v3(0,0,0),     -- angular velocity
                   v3(0,0,0),     -- force
                   v3(0,0,0),     -- torque
                   num(18),       -- mass
                   v3(2,7,5)      -- dims
                 )
  end
    for k=0,n_levels do
      var off = num((k+1)%2)/2
      for j=0,n_ring do
        var ang     = 2*math.pi/(n_ring) * (j+1+off)
        var rang    = 0.5*-ang
        var x       = radius*C.cos(ang)
        var z       = radius*C.sin(ang)
        boxes:loadrow( v3(x,1.0*k+0.5,z),
                       q4(0,C.sin(rang),0,C.cos(rang)),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       num(1),
                       v3(2,1,2)
                     )
      end
    end
  boxes:endload()

  return 300
end

local terra loadBoxesRoundTower2( store : API.Store )
  var boxes = store:Planks()

  var n_levels  = 50
  var n_ring    = 48
  var radius    = 22

  var n_extra   = 1
  if use_projectile then n_extra = 2 end

  boxes:beginload( n_levels*n_ring + n_extra )
    -- ground box
    boxes:loadrow( v3(0,-50,0),
                   q4(0,0,0,1),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   num(0),
                   v3(200,100,200)
                 )
  if use_projectile then
    -- projectile box
    boxes:loadrow( v3(30,28.75,0),   -- position
                   q4(0,0,0,1),   -- rotation
                   v3(-60,-30,0), -- linear velocity
                   v3(0,0,0),     -- angular velocity
                   v3(0,0,0),     -- force
                   v3(0,0,0),     -- torque
                   num(18),       -- mass
                   v3(2,7,5)      -- dims
                 )
  end
    for k=0,n_levels do
      var off = num((k+1)%2)/2
      for j=0,n_ring do
        var ang     = 2*math.pi/(n_ring) * (j+1+off)
        var rang    = 0.5*-ang
        var x       = radius*C.cos(ang)
        var z       = radius*C.sin(ang)
        boxes:loadrow( v3(x,1.0*k+0.5,z),
                       q4(0,C.sin(rang),0,C.cos(rang)),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       num(1),
                       v3(2,1,2)
                     )
      end
    end
  boxes:endload()

  return 300
end

local terra loadBoxesPlankTower1( store : API.Store )
  var boxes = store:Planks()

  var n_levels  = 25
  var n_ring    = 4
  var p_len     = 8.0f
  var p_width   = 0.5f
  var p_height  = 2.0f
  var p_off     = 1.0f

  var n_extra   = 1
  if use_projectile then n_extra = 2 end

  boxes:beginload( n_levels*n_ring/2 + n_extra )
    -- ground box
    boxes:loadrow( v3(0,-50,0),
                   q4(0,0,0,1),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   num(0),
                   v3(200,100,200)
                 )
  if use_projectile then
    -- projectile box
    boxes:loadrow( v3(10,16,5),   -- position
                   q4(0,0,0,1),   -- rotation
                   v3(-10,0,-5), -- linear velocity
                   v3(0,0,0),     -- angular velocity
                   v3(0,0,0),     -- force
                   v3(0,0,0),     -- torque
                   num(18),       -- mass
                   v3(4,4,4)      -- dims
                 )
  end
    for k=0,n_levels do
      var y     = p_height*k + (p_height/2.0f)
      var off   = p_len/2.0f - p_off
      if k%2 == 0 then -- place planks running in the z-direction
        boxes:loadrow( v3(-off,y,0),
                       q4(0,0,0,1),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       num(1),
                       v3(p_width,p_height,p_len)
                     )
        boxes:loadrow( v3(off,y,0),
                       q4(0,0,0,1),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       num(1),
                       v3(p_width,p_height,p_len)
                     )
      else -- place planks running in the x-direction
        boxes:loadrow( v3(0,y,-off),
                       q4(0,0,0,1),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       num(1),
                       v3(p_len,p_height,p_width)
                     )
        boxes:loadrow( v3(0,y,off),
                       q4(0,0,0,1),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       v3(0,0,0),
                       num(1),
                       v3(p_len,p_height,p_width)
                     )
      end
    end
  boxes:endload()

  return 300
end

local terra loadBoxesPlankTower2( store : API.Store )
  var boxes = store:Planks()

  var n_levels  = 25
  var p_len     = 8.0f
  var p_width   = 0.5f
  var p_height  = 2.0f
  var p_off     = 0.25f

  var tower_gap = 8.0f
  var tower_x   = 5
  var tower_z   = 3

  var n_extra   = 1
  if use_projectile then n_extra = 2 end

  boxes:beginload( n_levels*tower_x*tower_z*4/2 + n_extra )
    -- ground box
    boxes:loadrow( v3(0,-50,0),
                   q4(0,0,0,1),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   v3(0,0,0),
                   num(0),
                   v3(200,100,200)
                 )
  if use_projectile then
    -- projectile box
    boxes:loadrow( v3(-20,20,-10),   -- position
                   q4(0,0,0,1),   -- rotation
                   v3(30,0,20), -- linear velocity
                   v3(0,0,0),     -- angular velocity
                   v3(0,0,0),     -- force
                   v3(0,0,0),     -- torque
                   num(30),       -- mass
                   v3(5,5,5)      -- dims
                 )
  end
    for x_t = 0,tower_x do
    for z_t = 0,tower_z do
      for k=0,n_levels do
        var y     = p_height*k + (p_height/2.0f)
        var x     = tower_gap * x_t
        var z     = tower_gap * z_t
        var off   = p_len/2.0f - p_off
        if k%2 == 0 then -- place planks running in the z-direction
          boxes:loadrow( v3(x-off,y,z),
                         q4(0,0,0,1),
                         v3(0,0,0),
                         v3(0,0,0),
                         v3(0,0,0),
                         v3(0,0,0),
                         num(1),
                         v3(p_width,p_height,p_len)
                       )
          boxes:loadrow( v3(x+off,y,z),
                         q4(0,0,0,1),
                         v3(0,0,0),
                         v3(0,0,0),
                         v3(0,0,0),
                         v3(0,0,0),
                         num(1),
                         v3(p_width,p_height,p_len)
                       )
        else -- place planks running in the x-direction
          boxes:loadrow( v3(x,y,z-off),
                         q4(0,0,0,1),
                         v3(0,0,0),
                         v3(0,0,0),
                         v3(0,0,0),
                         v3(0,0,0),
                         num(1),
                         v3(p_len,p_height,p_width)
                       )
          boxes:loadrow( v3(x,y,z+off),
                         q4(0,0,0,1),
                         v3(0,0,0),
                         v3(0,0,0),
                         v3(0,0,0),
                         v3(0,0,0),
                         num(1),
                         v3(p_len,p_height,p_width)
                       )
        end
      end
    end end
  boxes:endload()

  return 500
end


------------------------------------------------------------------------------
-- PGS Solver

local SOLVER = nil
if solver_use == 'pgs' then
  SOLVER = PGS.GeneratePGSSolver(API, {
    timestep                = timestep,
    friction_coefficient    = friction_coefficient,
    gravity_acc             = gravity_acc,
    max_iters               = max_iters,
  })
else
  SOLVER = PCG.GeneratePCGSolver(API, {
    timestep                = timestep,
    friction_coefficient    = friction_coefficient,
    gravity_acc             = gravity_acc,
    max_iters               = max_iters,
  })
end

------------------------------------------------------------------------------
-- Visualization


local light = `v3([1/math.sqrt(6)],[1/math.sqrt(3)],[1/math.sqrt(2)])
local terra color_side( n : vec3 )
  var c = n:dot(light)
  c = num(0.5)*(c+num(1))
  vdb.color(c, c, c)
end
local vdb_line  = macro(function(x,y)
  return `vdb.line( x(0),x(1),x(2), y(0),y(1),y(2) )
end)
local vdb_tri   = macro(function(x,y,z)
  return `vdb.triangle( x(0),x(1),x(2), y(0),y(1),y(2), z(0),z(1),z(2) )
end)

local terra draw_boxes( store : API.Store )
  var n_box = store:Planks():getsize()
  var pos   = store:Planks():pos():read_lock()
  var rot   = store:Planks():rot():read_lock()
  var dims  = store:Planks():dims():read_lock()

  for b=0,n_box do
    -- compute the box frame vectors
    var nx    = rot[b]:rotvec(v3(1,0,0))
    var ny    = rot[b]:rotvec(v3(0,1,0))
    var nz    = rot[b]:rotvec(v3(0,0,1))
    var x     = (0.5 * dims[b](0) + 0.00) * nx
    var y     = (0.5 * dims[b](1) + 0.00) * ny
    var z     = (0.5 * dims[b](2) + 0.00) * nz
    var p     = pos[b]
    --[[
               c5       * ---- *        c1
                      / |    / |
          c4        * ---- *   |   c0
               c7   |   * -|-- *        c3
                    | /    | /
          c6        * ---- *       c2
    --]]
    var c0 = p + x + y + z
    var c1 = p + x + y - z
    var c2 = p + x - y + z
    var c3 = p + x - y - z
    var c4 = p - x + y + z
    var c5 = p - x + y - z
    var c6 = p - x - y + z
    var c7 = p - x - y - z

    --C.printf('  box #%d:  %f %f %f\n', b, p(0), p(1), p(2))

    if b ~= 0 then -- exclude ground box
      -- line drawing
      if debug_draw then
        vdb.color(1,1,1)
        vdb_line(c0,c1)  vdb_line(c1,c3)  vdb_line(c3,c2)  vdb_line(c2,c0)
        vdb_line(c4,c5)  vdb_line(c5,c7)  vdb_line(c7,c6)  vdb_line(c6,c4)
        vdb_line(c0,c4)  vdb_line(c1,c5)  vdb_line(c2,c6)  vdb_line(c3,c7)
      end
      -- face drawing
      if not debug_draw then
        color_side(nx)   vdb_tri(c0, c2, c1)  vdb_tri(c2, c3, c1)
        color_side(-nx)  vdb_tri(c4, c5, c7)  vdb_tri(c7, c6, c4)
        color_side(ny)   vdb_tri(c0, c1, c4)  vdb_tri(c1, c5, c4)
        color_side(-ny)  vdb_tri(c3, c2, c6)  vdb_tri(c6, c7, c3)
        color_side(nz)   vdb_tri(c0, c4, c6)  vdb_tri(c6, c2, c0)
        color_side(-nz)  vdb_tri(c1, c3, c7)  vdb_tri(c7, c5, c1)
      end
    end
  end

  store:Planks():pos():read_unlock()
  store:Planks():rot():read_unlock()
  store:Planks():dims():read_unlock()
end

local terra print_boxes( store : API.Store )
  var n_box = store:Planks():getsize()
  var pos   = store:Planks():pos():read_lock()
  var vel   = store:Planks():linvel():read_lock()
  var rot   = store:Planks():rot():read_lock()
  var dims  = store:Planks():dims():read_lock()

  for b=0,n_box do
    -- compute the box frame vectors
    var p     = pos[b]
    var v     = vel[b]

    C.printf('  box #%d:  %f %f %f  ;  %f %f %f\n',
              b, p(0), p(1), p(2), v(0), v(1), v(2) )
  end

  store:Planks():pos():read_unlock()
  store:Planks():linvel():read_unlock()
  store:Planks():rot():read_unlock()
  store:Planks():dims():read_unlock()
end

local terra draw_contacts( store : API.Store )
  var nC_alloc  = store:PPContacts():get_n_alloc()
  var is_live   = store:PPContacts():is_live():read_lock()
  var n_pts     = store:PPContacts():n_pts():read_lock()
  var pts       = store:PPContacts():pts():read_lock()
  var basis     = store:PPContacts():basis():read_lock()

  var c_b0      = store:PPContacts():p0():read_lock()
  var c_b1      = store:PPContacts():p1():read_lock()

  var pos       = store:Planks():pos():read_lock()
  var rot       = store:Planks():rot():read_lock()

  for c=0,nC_alloc do if is_live[c] then
    --C.printf('    drawing contact %d\n', c)
    for k=0,n_pts[c] do
      var b0,b1     = c_b0[c], c_b1[c]
      var l0,l1     = pts[c].d[k].local0, pts[c].d[k].local1
      var pt0, pt1  = rot[b0]:rotvec(l0) + pos[b0],
                      rot[b1]:rotvec(l1) + pos[b1]

      var p     = pts[c].d[k].pt
      var np    = basis[c].norm + p

      --vdb.color(0,1,0)
      --vdb.point(pt0(0),pt0(1),pt0(2))
      --vdb.color(0,0,1)
      --vdb.point(pt1(0),pt1(1),pt1(2))
      vdb.color(1,0,0)
      vdb.point(p(0),p(1),p(2))
      vdb.color(1,1,0)
      vdb_line(p, np)
    end
  end end

  store:Planks():pos():read_unlock()
  store:Planks():rot():read_unlock()

  store:PPContacts():p0():read_unlock()
  store:PPContacts():p1():read_unlock()

  store:PPContacts():is_live():read_unlock()
  store:PPContacts():n_pts():read_unlock()
  store:PPContacts():pts():read_unlock()
  store:PPContacts():basis():read_unlock()
end




------------------------------------------------------------------------------
-- Sim Loop


local terra drawStore( store : API.Store )
  vdb.vbegin()
  vdb.frame()

  if debug_draw then draw_contacts(store) end
  draw_boxes(store)

  vdb.vend()
  --C.getchar()
end

local terra dumpStore( store : API.Store, filename : rawstring )
  var F   = C.fopen(filename, 'w')
  assert(F ~= nil, 'failed to open file %s for writing', filename)
  C.printf('Opened %s for dumping...\n', filename)

  var n_alloc         = store:PPContacts():get_n_alloc()
  var n_contacts      = store:PPContacts():get_n_rows()
  var n_box           = store:Planks():getsize()
  var pos             = store:Planks():pos():read_lock()
  var rot             = store:Planks():rot():read_lock()
  var dims            = store:Planks():dims():read_lock()
  var p0, p1          = store:PPContacts():p0():read_lock(),
                        store:PPContacts():p1():read_lock()
  var is_live         = store:PPContacts():is_live():read_lock()

  var n_pts           = store:PPContacts():n_pts():read_lock()
  var basis           = store:PPContacts():basis():read_lock()
  var pts             = store:PPContacts():pts():read_lock()

  C.fprintf(F, "n_boxes:        %5d\n", n_box)
  C.fprintf(F, "n_contacts:     %5d\n", n_contacts)
  C.fprintf(F, "\nBOXES\n")
  for b=0,n_box do
    C.fprintf(F, "  %5d : %8.4f %8.4f %8.4f ; %8.4f %8.4f %8.4f %8.4f\n",
                 b,
                 pos[b].d[0], pos[b].d[1], pos[b].d[2],
                 rot[b].d[0], rot[b].d[1], rot[b].d[2], rot[b].d[3] )
  end
  C.fprintf(F, "\nCONTACTS\n")
  var p_tmp           = [&uint64](C.malloc(n_contacts*sizeof(uint64)))
  var w               = 0
  for c=0,n_alloc do
    if is_live[c] then
      var packed      = ([uint64](p0[c]) << 32) or [uint64](p1[c])
      p_tmp[w]        = packed
      w               = w + 1
      assert(w <= n_contacts, 'Found too many contacts; N:%d alloc:%d c:%d',
             n_contacts, n_alloc, c)
    end
  end
  assert(w == n_contacts, 'Found wrong number of contacts')
  --sort(n_contacts, p_tmp)
  for c=0,n_contacts do
    var packed        = p_tmp[c]
    var i0, i1        = [uint32](packed >> 32), [uint32](packed)
    C.fprintf(F, "%5d (%d): %5d %5d ; %8.4f %8.4f %8.4f\n",
                 c, n_pts[c], i0, i1,
                 basis[c].norm.d[0], basis[c].norm.d[1], basis[c].norm.d[2] )
    for k=0,n_pts[c] do
      C.fprintf(F, "      [%d]: %8.4f %8.4f %8.4f ; %8.4f\n", k,
        pts[c].d[k].pt.d[0], pts[c].d[k].pt.d[1], pts[c].d[k].pt.d[2],
        pts[c].d[k].depth )
      C.fprintf(F, "           %8.4f %8.4f %8.4f ; %8.4f %8.4f %8.4f\n",
        pts[c].d[k].rel0.d[0], pts[c].d[k].rel0.d[1], pts[c].d[k].rel0.d[2],
        pts[c].d[k].rel1.d[0], pts[c].d[k].rel1.d[1], pts[c].d[k].rel1.d[2] )
      C.fprintf(F, "           %8.4f %8.4f %8.4f ; %8.4f %8.4f %8.4f\n",
   pts[c].d[k].local0.d[0], pts[c].d[k].local0.d[1], pts[c].d[k].local0.d[2],
   pts[c].d[k].local1.d[0], pts[c].d[k].local1.d[1], pts[c].d[k].local1.d[2] )
    end
  end
  C.free(p_tmp)
  --{ 'pt',                 vec3 },
  --{ 'rel0',               vec3 },
  --{ 'rel1',               vec3 },
  --{ 'local0',             vec3 },
  --{ 'local1',             vec3 },
  --{ 'depth',              num },

  store:PPContacts():pts():read_unlock()
  store:PPContacts():basis():read_unlock()
  store:PPContacts():n_pts():read_unlock()

  store:PPContacts():is_live():read_unlock()
  store:PPContacts():p0():read_unlock()
  store:PPContacts():p1():read_unlock()
  store:Planks():pos():read_unlock()
  store:Planks():rot():read_unlock()
  store:Planks():dims():read_unlock()

  C.printf('Done dumping to %s\n', filename)
  assert(C.fclose(F) == 0, 'failed to close file %s', filename)
end

local terra load_frame_from_file(
  load_dir : rawstring,
  frame_no : int,
  store : API.Store
)
  var filename :int8[1024]
  C.snprintf( filename, 1024, "%s/frame_%d.txt", load_dir, frame_no )
  var F   = C.fopen(filename, 'r')
  assert(F ~= nil, 'failed to open file %s for reading', filename)

  var n_rows : uint
  var d : float[3]
  var p : float[3]
  var r : float[4]
  var v : float[3]
  var a : float[3]
  C.fscanf(F, "%d", &n_rows)
  --C.printf("Found %d rows\n", n_rows)

  var n_box           = store:Planks():getsize()
  assert(n_box == n_rows, 'expected %d rows, but read %d', n_box, n_rows)
  var dims            = store:Planks():dims():readwrite_lock()
  var pos             = store:Planks():pos():readwrite_lock()
  var rot             = store:Planks():rot():readwrite_lock()
  var linvel          = store:Planks():linvel():readwrite_lock()
  var angvel          = store:Planks():angvel():readwrite_lock()

  --C.printf("proj pos: %f %f %f\n", pos[1].d[0], pos[1].d[1], pos[1].d[2])
  --C.printf("     vel: %f %f %f\n",
  --        linvel[1].d[0], linvel[1].d[1], linvel[1].d[2])
  --
  --C.printf("  box 0 rot: %f %f %f %f\n",
  --      rot[2].d[0], rot[2].d[1], rot[2].d[2], rot[2].d[3])

  for i=0,n_rows do
    C.fscanf(F, "%f %f %f ; %f %f %f ; %f %f %f %f ; %f %f %f ; %f %f %f",
                d, d+1, d+2, p, p+1, p+2, r, r+1, r+2, r+3,
                v, v+1, v+2, a, a+1, a+2
      --           dims[i].d, dims[i].d+1, dims[i].d+2,
      --           pos[i].d, pos[i].d+1, pos[i].d+2,
      --           rot[i].d, rot[i].d+1, rot[i].d+2, rot[i].d+3,
      --           linvel[i].d, linvel[i].d+1, linvel[i].d+2,
      --           angvel[i].d, angvel[i].d+1, angvel[i].d+2
            )
    --C.printf("row %d done\n", i)
    escape for j=0,2 do emit quote
      --dims[i].d[j] = d[j] * 2 -- adjust dimension convention
      pos[i].d[j]  = p[j]
      rot[i].d[j]  = r[j]
      linvel[i].d[j]  = v[j]
      angvel[i].d[j]  = a[j]
    end end end
    rot[i].d[3] = r[3]
  end

  store:Planks():dims():readwrite_unlock()
  store:Planks():pos():readwrite_unlock()
  store:Planks():rot():readwrite_unlock()
  store:Planks():linvel():readwrite_unlock()
  store:Planks():angvel():readwrite_unlock()
  assert(C.fclose(F) == 0, 'failed to close file %s', filename)
end

local terra mainLoop()
  C.printf('        -+-+- CODE: mainLoop() -+- Begin\n')
  var store       = API.NewStore()
  C.printf('        -+-+- CODE: mainLoop() -+- into loadBoxes()\n')
  var N_FRAMES    : uint32
  escape if test_case == 'slant_ground' then emit quote
    N_FRAMES      = loadBoxesSlantGround(store)
  end elseif test_case == 'simple_stack' then emit quote
    N_FRAMES      = loadBoxesSimpleStack(store)
  end elseif test_case == 'round_tower' then emit quote
    N_FRAMES      = loadBoxesRoundTower(store)
  end elseif test_case == 'round_tower0' then emit quote
    N_FRAMES      = loadBoxesRoundTower0(store)
  end elseif test_case == 'round_tower1' then emit quote
    N_FRAMES      = loadBoxesRoundTower1(store)
  end elseif test_case == 'round_tower2' then emit quote
    N_FRAMES      = loadBoxesRoundTower2(store)
  end elseif test_case == 'plank_tower1' then emit quote
    N_FRAMES      = loadBoxesPlankTower1(store)
  end elseif test_case == 'plank_tower2' then emit quote
    N_FRAMES      = loadBoxesPlankTower2(store)
  end else
    error('unrecognized test-case: '..tostring(test_case))
  end end
  loadContacts(store)

    vdb.vbegin()
    vdb.frame()

    --draw_boxes(store)

    vdb.vend()

  C.printf('        -+-+- CODE: mainLoop() -+- alloc solver\n')
  var solver : SOLVER
  solver:alloc(store)

  for k=0,N_FRAMES do
    --var namebuf :int8[1024]
    --var file_stubname   = [ (use_bvh and "../../bvh_data/dump")
    --                                  or "../../scan_data/dump" ]
    --C.snprintf( namebuf, 1024, "%s%04d.txt", file_stubname, k )
    --dumpStore( store, namebuf )

    escape if load_dir then emit quote
      load_frame_from_file(load_dir, k, store)
    end end end

    C.printf('timestep #%d:\n', k)
    var start     = taketime()
    solver:do_timestep(nil)
    var stop      = taketime()
    var wait_time = timestep - (stop-start)
    if wait_time > 0 then
      C.usleep([int](wait_time*1e6))
    end

    if ([int32](N_FRAMES-1)-[int32](k))%60 == 0 then
      drawStore(store)
    end
    --print_boxes(store)
  end

  store:print_profile()
  solver:print_final_report()

  C.printf('        -+-+- CODE: mainLoop() -+- Start Free\n')
  solver:free()
  store:destroy()
  C.printf('        -+-+- CODE: mainLoop() -+- End\n')
end



mainLoop()















