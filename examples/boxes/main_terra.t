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
                 max_iters      = 50,
               }
for _,a in ipairs(arg) do
  local _, _, label, value = a:find("^%-([^=]*)=(.*)$")
  if label and value then params[label] = value end
end

local debug_draw            = boolparam(params.debug_draw)

local traversal             = params.traversal
local collide_shape         = params.collide_shape
local test_case             = params.test_case
local use_projectile        = not boolparam(params.no_projectile)
local effect_buffer         = boolparam(params.effect_buffer)
local index_buffer          = boolparam(params.index_buffer)
local verify                = boolparam(params.verify)

local timestep              = 1/60
local inv_timestep          = 1/timestep

local friction_coefficient  = 0.30
local gravity_acc           = 9.8

local mass                  = 1
local invmass               = 1/mass

local max_iters             = tonumber(params.max_iters)

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
    elseif collide_shape == 'dop' then
      Boxes.find_plank_iscts:set_cpu_traversal(Boxes.aabb_bvh_traversal)
    else error('unrecognized collision shape: '..collide_shape)
    end
  elseif traversal == 'hash_split' then
    local cell_w = 3
    if test_case == 'slant_ground' then
    elseif test_case == 'simple_stack' then
    elseif test_case == 'round_tower' then
    elseif test_case == 'plank_tower' then
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

  local API = G.CompileLibrary {
    tables            = Boxes.tables,
    joins             = Boxes.joins,
    terra_out         = true,
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
    boxes:loadrow( v3(20,14.5,0),   -- position
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
  var pos   = store:Planks():pos():readwrite_lock()
  var rot   = store:Planks():rot():readwrite_lock()
  var dims  = store:Planks():dims():readwrite_lock()

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

  store:Planks():pos():readwrite_unlock()
  store:Planks():rot():readwrite_unlock()
  store:Planks():dims():readwrite_unlock()
end

local terra print_boxes( store : API.Store )
  var n_box = store:Planks():getsize()
  var pos   = store:Planks():pos():readwrite_lock()
  var vel   = store:Planks():linvel():readwrite_lock()
  var rot   = store:Planks():rot():readwrite_lock()
  var dims  = store:Planks():dims():readwrite_lock()

  for b=0,n_box do
    -- compute the box frame vectors
    var p     = pos[b]
    var v     = vel[b]

    C.printf('  box #%d:  %f %f %f  ;  %f %f %f\n',
              b, p(0), p(1), p(2), v(0), v(1), v(2) )
  end

  store:Planks():pos():readwrite_unlock()
  store:Planks():linvel():readwrite_unlock()
  store:Planks():rot():readwrite_unlock()
  store:Planks():dims():readwrite_unlock()
end

local terra draw_contacts( store : API.Store )
  var nC_alloc  = store:PPContacts():get_n_alloc()
  var is_live   = store:PPContacts():is_live():read_lock()
  var n_pts     = store:PPContacts():n_pts():readwrite_lock()
  var pts       = store:PPContacts():pts():readwrite_lock()
  var basis     = store:PPContacts():basis():readwrite_lock()

  var c_b0      = store:PPContacts():p0():readwrite_lock()
  var c_b1      = store:PPContacts():p1():readwrite_lock()

  var pos       = store:Planks():pos():readwrite_lock()
  var rot       = store:Planks():rot():readwrite_lock()

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

  store:Planks():pos():readwrite_unlock()
  store:Planks():rot():readwrite_unlock()

  store:PPContacts():p0():readwrite_unlock()
  store:PPContacts():p1():readwrite_unlock()

  store:PPContacts():is_live():read_unlock()
  store:PPContacts():n_pts():readwrite_unlock()
  store:PPContacts():pts():readwrite_unlock()
  store:PPContacts():basis():readwrite_unlock()
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
  end elseif test_case == 'plank_tower' then emit quote
    N_FRAMES      = loadBoxesPlankTower1(store)
  end elseif test_case == 'plank_tower2' then emit quote
    N_FRAMES      = loadBoxesPlankTower2(store)
  end else
    error('unrecognized test-case: '..tostring(test_case))
  end end

    vdb.vbegin()
    vdb.frame()

    draw_boxes(store)

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

    C.printf('timestep #%d:\n', k)
    var start     = taketime()
    solver:do_timestep(nil)
    var stop      = taketime()
    var wait_time = timestep - (stop-start)
    if wait_time > 0 then
      C.usleep([int](wait_time*1e6))
    end

    drawStore(store)
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















