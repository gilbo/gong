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

local debug_draw            = true

local timestep              = 1/60
local inv_timestep          = 1/timestep

local friction_coefficient  = 0.30
local gravity_acc           = 9.8

local mass                  = 1
local invmass               = 1/mass

local max_iters             = 50

-- NOTE: The PCG solver appears to be broken
--        I don't know what's wrong with it...
local solver_use            = 'pgs'

------------------------------------------------------------------------------
-- API Generation

local function GenBoxAPI()
  local Boxes             = require 'boxes'

  local API = G.CompileLibrary {
    tables          = Boxes.tables,
    joins           = Boxes.joins,
    terra_out       = true,
  }

  local blockout = {
    tables        = true,
    joins         = true,
    Planks        = true,
    PPContacts    = true,
    find_plank_iscts = true,
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
end

local terra loadBoxesRoundTower( store : API.Store )
  var boxes = store:Planks()

  var n_levels  = 50
  var n_ring    = 24
  var radius    = 11

  boxes:beginload( n_levels*n_ring + 2 )
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
    -- projectile box
    boxes:loadrow( v3(20,14,0),   -- position
                   q4(0,0,0,1),   -- rotation
                   v3(-60,-30,0), -- linear velocity
                   v3(0,0,0),     -- angular velocity
                   v3(0,0,0),     -- force
                   v3(0,0,0),     -- torque
                   num(15),       -- mass
                   v3(2,7,5)      -- dims
                 )
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
  C.getchar()
end

local terra mainLoop()
  C.printf('        -+-+- CODE: mainLoop() -+- Begin\n')
  var store       = API.NewStore()
  C.printf('        -+-+- CODE: mainLoop() -+- into loadBoxes()\n')
  --loadBoxesSlantGround(store)
  loadBoxesSimpleStack(store)
  --loadBoxesRoundTower(store)

    vdb.vbegin()
    vdb.frame()

    draw_boxes(store)

    vdb.vend()

  C.printf('        -+-+- CODE: mainLoop() -+- alloc solver\n')
  var solver : SOLVER
  solver:alloc(store)

  for k=0,300 do
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

  C.printf('        -+-+- CODE: mainLoop() -+- Start Free\n')
  solver:free()
  store:destroy()
  C.printf('        -+-+- CODE: mainLoop() -+- End\n')
end



mainLoop()















