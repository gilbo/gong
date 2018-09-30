import 'gong'
local G = gong.stdlib

local Exports = {}
package.loaded["PGS_terra"] = Exports


local newlist = terralib.newlist

local Prelude   = require 'prelude'
local C         = Prelude.C
local taketime  = Prelude.taketime
local minf      = Prelude.minf
local maxf      = Prelude.maxf
local clampf    = Prelude.clampf

local assert  = G.cassert

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- PGS Solver

local function GeneratePGSSolver(API, params)
  Prelude.API_Extend(API)

  local vec3    = API.vec3:terratype()
  local quat    = API.quat:terratype()
  local mat3    = API.mat3:terratype()
  local num     = API.num:terratype()
  local numEPS  = Prelude.FLT_EPSILON
  if num == double then numEPS = C.DBL_EPSILON end

  -- useful constructor function/macros
  local v3 = macro(function(x,y,z)
    return `vec3{array(num(x),num(y),num(z))} end)
  local m3 = macro(function(a00,a01,a02, a10,a11,a12, a20,a21,a22)
    return `mat3{array(num(a00),num(a10),num(a20),
                       num(a01),num(a11),num(a21),
                       num(a02),num(a12),num(a22))} end)
  local q4 = macro(function(x,y,z,w)
    return `quat{array(num(x),num(y),num(z),num(w))} end)

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  local ContactT    = (API.ContactT[4]):terratype()
  local ContactBase = (API.ContactBase):terratype()

  -- useful constructor function/macros
  local v3 = macro(function(x,y,z)
    return `vec3{array(num(x),num(y),num(z))} end)
  local m3 = macro(function(a00,a01,a02, a10,a11,a12, a20,a21,a22)
    return `mat3{array(num(a00),num(a10),num(a20),
                       num(a01),num(a11),num(a21),
                       num(a02),num(a12),num(a22))} end)
  local q4 = macro(function(x,y,z,w)
    return `quat{array(num(x),num(y),num(z),num(w))} end)

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  local timestep          = assert(params.timestep, 'must supply timestep')
  local inv_timestep      = constant(num, 1/timestep)
        timestep          = constant(num, timestep)

  -- I believe 0.2 is accurate to Bullet's default setting...
  -- This is called a Baumgarte parameter.
  local baumgarte         = params.baumgarte or 0.8
  local beta_penetration  = constant(num, baumgarte * inv_timestep:asvalue())
  local split_pen_threshold   = constant(num, 0.04)
  local warmstart_factor  = constant(num, 0.85)

  local friction          = params.friction or 0.25
  local gravity_acc       = params.gravity_acc or 9.8
  friction                = constant(num, friction)
  gravity_acc             = constant(num, gravity_acc)

  local max_iters         = params.max_iters or 50

  local SIMD_EPS          = constant(num, numEPS)
  local EPS               = constant(num, 1e-5)

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  local struct Histogram {
    _sum     : double
    _min     : double
    _max     : double
    _count   : uint32
  }
  terra Histogram:init()
    self._sum   = 0
    self._min   = [math.huge]
    self._max   = [-math.huge]
    self._count = 0
  end
  terra Histogram:acc(v : double)
    self._sum   = self._sum + v
    if v < self._min then self._min = v end
    if v > self._max then self._max = v end
    self._count = self._count + 1
  end
  terra Histogram:avg()
    return self._sum / [double](self._count)
  end
  terra Histogram:min()   return self._min    end
  terra Histogram:max()   return self._max    end
  terra Histogram:count() return self._count  end
  terra Histogram:sum()   return self._sum    end

  local struct PGS_State {
    -- scalar solver data
    --alpha_numerator : num
    --denominator     : num
    --beta            : num
    -- data sizes
    n_boxes         : int
    n_contacts      : int
    n_c_alloc       : int
    -- Boxes-parallel data
    --tmp_linvel      : &vec3
    --tmp_angvel      : &vec3
    inv_mass        : &num
    inv_inertia     : &mat3
    --
    linvel_prev     : &vec3
    angvel_prev     : &vec3
    pos_prev        : &vec3
    rot_prev        : &quat
    --
    d_linvel        : &vec3
    d_angvel        : &vec3
    push            : &vec3
    turn            : &vec3
    --
    pos             : &vec3
    rot             : &quat
    linvel          : &vec3
    angvel          : &vec3
    force           : &vec3
    torque          : &vec3
    mass            : &num
    dims            : &vec3
    -- Contact-parallel 4x data
    rhs             : &num
    rhs_fric        : &num
    rhsPos          : &num
    fric_limit      : &num  -- superfluous
    inv_J           : &num
    inv_J_fric      : &num
    --
    l_mult          : &num
    fric_mult       : &num
    pos_mult        : &num
    --
    friction_dir    : &vec3
    -- Contact-parallel 1x data
    is_live         : &bool
    b0              : &uint
    b1              : &uint
    n_pts           : &uint
    pts             : &ContactT
    basis           : &ContactBase

    store           : API.Store

    -- Stats
    total_time      : Histogram
    collide_time    : Histogram
    solve_time      : Histogram
  }

  local terra getAllocLimit( store : API.Store )
    var cs              = store:PPContacts()
    var n_alloc         = cs:get_n_alloc()
    var n_rows          = cs:get_n_rows()
    -- now figure out the upper bound
    var is_live         = cs:is_live():read_lock()
    var LIMIT           = 0
    while LIMIT < n_alloc do
      if is_live[LIMIT] then
        n_rows  = n_rows - 1
      end
      LIMIT     = LIMIT + 1
      if n_rows == 0 then
        break
      end
    end
    cs:is_live():read_unlock()
    return LIMIT
  end

  terra PGS_State:alloc( store : API.Store )
    self.store          = store

    self.total_time:init()
    self.collide_time:init()
    self.solve_time:init()

    var bs              = store:Planks()
    var cs              = store:PPContacts()
    var nB              = bs:getsize()
    self.n_boxes        = nB
    self.n_contacts     = cs:get_n_rows()
    self.n_c_alloc      = getAllocLimit(store)
    var nC              = self.n_c_alloc

    -- box parallel additional
    escape for _,data in ipairs{
      {'inv_mass',    num },  {'inv_inertia', mat3},
      {'linvel_prev', vec3},  {'angvel_prev', vec3},
      {'d_linvel',    vec3},  {'d_angvel',    vec3},
      {'push',        vec3},  {'turn',        vec3},
      {'pos_prev',    vec3},  {'rot_prev',    quat},
    } do
      local name, typ = unpack(data)
    emit quote
      self.[name]       = [&typ](C.malloc( nB * sizeof(typ) ))
    end end end

    -- box parallel from store
    escape for _,name in ipairs{
      'pos', 'rot', 'linvel', 'angvel', 'force', 'torque', 'mass', 'dims',
    } do emit quote
      self.[name]       = bs:[name]():readwrite_lock()
    end end end

    -- contact parallel additional
    escape for _,name in ipairs{
      'rhs', 'rhs_fric', 'rhsPos',
      'fric_limit', 'inv_J', 'inv_J_fric',
      'pos_mult'
    } do emit quote
      self.[name]       = [&num](C.malloc( nC * 4 * sizeof(num) ))
    end end end
    self.friction_dir   = [&vec3](C.malloc( nC * 4 * sizeof(vec3) ))

    -- contact-parallel 4x with cast from store
    self.l_mult         = [&num]( cs:l_mult():readwrite_lock() )
    self.fric_mult      = [&num]( cs:fric_mult():readwrite_lock() )

    -- contact parallel from store
    self.b0             = cs:p0():readwrite_lock()
    self.b1             = cs:p1():readwrite_lock()
    self.is_live        = cs:is_live():read_lock()
    escape for _,name in ipairs{
      'n_pts', 'pts', 'basis',
    } do emit quote
      self.[name]       = cs:[name]():readwrite_lock()
    end end end
  end
  terra PGS_State:unlock_store()
    var bs              = self.store:Planks()
    var cs              = self.store:PPContacts()
    escape for _,name in ipairs{
      'pos', 'rot', 'linvel', 'angvel', 'force', 'torque', 'mass', 'dims',
    } do emit quote
      bs:[name]():readwrite_unlock()
    end end end

    cs:is_live():read_unlock()
    escape for _,name in ipairs{
      'n_pts', 'pts', 'basis', 'p0', 'p1', 'l_mult', 'fric_mult'
    } do emit quote
      cs:[name]():readwrite_unlock()
    end end end
  end
  terra PGS_State:relock_store()
    var bs              = self.store:Planks()
    assert( self.n_boxes == bs:getsize() )
    var cs              = self.store:PPContacts()
    self.n_contacts     = cs:get_n_rows()
    var newsize         = getAllocLimit(self.store)
    var oldsize         = self.n_contacts
    self.n_c_alloc      = newsize
    C.printf("    Found %d contacts (alloc %d)\n", self.n_contacts, newsize)

    -- box parallel from store
    escape for _,name in ipairs{
      'pos', 'rot', 'linvel', 'angvel', 'force', 'torque', 'mass', 'dims',
    } do emit quote
      self.[name]       = bs:[name]():readwrite_lock()
    end end end

    -- contact parallel additional
    escape for _,name in ipairs{
      'rhs', 'rhs_fric', 'rhsPos',
      'fric_limit', 'inv_J', 'inv_J_fric',
      'pos_mult'
    } do emit quote
      self.[name] = [&num](C.realloc( self.[name], newsize*4*sizeof(num) ))
    end end end
    self.friction_dir = [&vec3](C.realloc( self.friction_dir,
                                           newsize*4*sizeof(vec3) ))

    -- contact-parallel 4x with cast from store
    self.l_mult     = [&num]( cs:l_mult():readwrite_lock() )
    self.fric_mult  = [&num]( cs:fric_mult():readwrite_lock() )

    -- contact parallel from store
    self.b0         = cs:p0():readwrite_lock()
    self.b1         = cs:p1():readwrite_lock()
    self.is_live    = cs:is_live():read_lock()
    escape for _,name in ipairs{
      'n_pts', 'pts', 'basis',
    } do emit quote
      self.[name]   = cs:[name]():readwrite_lock()
    end end end
  end
  terra PGS_State:free()
    var bs              = self.store:Planks()
    var cs              = self.store:PPContacts()

    -- box parallel additional
    escape for _,name in ipairs{
      'inv_mass', 'inv_inertia',
      'd_linvel',    'd_angvel',    'push',     'turn',
      'linvel_prev', 'angvel_prev', 'pos_prev', 'rot_prev',
    } do emit quote
      C.free(self.[name])
      self.[name] = nil
    end end end

    -- box parallel from store
    escape for _,name in ipairs{
      'pos', 'rot', 'linvel', 'angvel', 'force', 'torque', 'mass', 'dims',
    } do emit quote
      self.[name] = nil
      bs:[name]():readwrite_unlock()
    end end end

    -- contact parallel additional
    escape for _,name in ipairs{
      'rhs', 'rhs_fric', 'rhsPos',
      'fric_limit', 'inv_J', 'inv_J_fric',
      'pos_mult',
      'friction_dir'
    } do emit quote
      C.free(self.[name])
      self.[name] = nil
    end end end

    -- contact parallel from store
    cs:p0():readwrite_unlock()
    cs:p1():readwrite_unlock()
    cs:is_live():read_unlock()
    escape for _,name in ipairs{
      'n_pts', 'pts', 'basis', 'l_mult', 'fric_mult'
    } do emit quote
      self.[name] = nil
      cs:[name]():readwrite_unlock()
    end end end
  end

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  terra PGS_State:print()
    --
    C.printf("  BOXES       #%d\n", self.n_boxes)
   C.printf(["           pos     -       -               ;"..
                       " linvel  -       -       \n"])
   C.printf(["           rot     -       -       -       ;"..
                       " angvel  -       -       \n"])
  --C.printf("           force   -       -       ; torque  -       -       \n")
    for b=0,self.n_boxes do
      var p, v = self.pos[b], self.linvel[b]
      var r, w = self.rot[b], self.angvel[b]
  --  var f, t = self.force[b], self.torque[b]
    C.printf("    %4d:  %7.3f %7.3f %7.3f         ; %7.3f %7.3f %7.3f\n",
             b, p(0), p(1), p(2), v(0), v(1), v(2))
    C.printf("           %7.3f %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n",
             r(0), r(1), r(2), r(3), w(0), w(1), w(2))
    end
    C.printf("  CONTACTS    #%d\n", self.n_contacts)
    C.printf("              l_mult  fricmlt pos_mlt ; rhs     rhsfric rhs_pos ; depth\n")
    C.printf("              fric_dir        -       ; invJ    invJfric\n")
    C.printf("              norm    -       -       ; ctct_pt -       -\n")
    --C.printf("              rel0    -       -       ; rel1    -       -\n")
    for c=0,self.n_c_alloc do if self.is_live[c] then
    C.printf("    %4d: (%d)  %4d, %4d\n", c, self.n_pts[c],
                                          self.b0[c], self.b1[c])
      for k=0,self.n_pts[c] do
        var lm, fm, pm  = self.l_mult[4*c+k], self.fric_mult[4*c+k],
                                              self.pos_mult[4*c+k]
        var  r, fr, pr  = self.rhs[4*c+k], self.rhs_fric[4*c+k],
                                           self.rhsPos[4*c+k]
        var d           = self.pts[c].d[k].depth
        var fd          = self.friction_dir[4*c+k]
        var iJ, iJf     = self.inv_J[4*c+k], self.inv_J_fric[4*c+k]
        var n           = self.basis[c].norm
        var pt          = self.pts[c].d[k].rel1 + self.pos_prev[ self.b1[c] ]
    C.printf("          %d : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f ; %7.3f\n",
             k, lm, fm, pm, r, fr, pr, d)
    C.printf("            : %7.3f %7.3f %7.3f ; %7.3f %7.3f\n",
             fd(0), fd(1), fd(2), iJ, iJf)
    C.printf("            : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n\n",
             n(0), n(1), n(2), pt(0), pt(1), pt(2))
      end
    end end
    --]]
  end

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  terra PGS_State:cacheInvMassMatrix()
    var rot         = self.rot
    var mass        = self.mass
    var dims        = self.dims

    for b=0,self.n_boxes do
      if mass[b] == 0 then
        self.inv_mass[b]      = 0
        self.inv_inertia[b]   = m3(0,0,0, 0,0,0, 0,0,0)
      else
        var m                 = mass[b]
        var d0, d1, d2        = dims[b](0), dims[b](1), dims[b](2)
        var qR                = rot[b]:toRotMat()
        var qcR               = rot[b]:conj():toRotMat()
        var I_inv             = m3(12/(m*(d1*d1 + d2*d2)), 0, 0,
                                   0, 12/(m*(d0*d0 + d2*d2)), 0,
                                   0, 0, 12/(m*(d0*d0 + d1*d1))
                                  )
        self.inv_mass[b]      = 1/m
        self.inv_inertia[b]   = qR * I_inv * qcR
      end
    end
  end

  terra PGS_State:gyroForce(b : uint32, dt : num)
    var m                 = self.mass[b]
    if m == num(0) then return v3(0,0,0) end

    var d0, d1, d2        = self.dims[b](0), self.dims[b](1), self.dims[b](2)
    var Inertia           = m3( (m*(d1*d1 + d2*d2))/12, 0, 0,
                                0, (m*(d0*d0 + d2*d2))/12, 0,
                                0, 0, (m*(d0*d0 + d1*d1))/12 )
    var w                 = self.angvel[b]
    var q                 = self.rot[b]

    var w_b               = q:conj():rotvec(w)
    var f                 = dt * ( w_b:cross(Inertia * w_b) )
    var skew0             = w_b:crossmat()
    var skew1             = (Inertia * w_b):crossmat()
    var J                 = Inertia + dt * (skew0*Inertia - skew1)

    var w_d               = J:invert() * f
    return - q:rotvec(w_d)
  end

  PGS_State.methods.ComputeFrictionDir = macro(function(pgs, ci, k)
    return quote
      var b0      = pgs.b0[ci]
      var b1      = pgs.b1[ci]
      var v0, v1  = pgs.linvel[b0], pgs.linvel[b1]
      var w0, w1  = pgs.angvel[b0], pgs.angvel[b1]

      var n       = pgs.basis[ci].norm
      var r0      = pgs.pts[ci].d[k].rel0
      var r1      = pgs.pts[ci].d[k].rel1

      var vel     = (v1 - v0 + w1:cross(r1) - w0:cross(r0))
      var lat     = vel - n:dot(vel) * n
      var fdir    = lat
      if lat:dot(lat) > SIMD_EPS then
        fdir      = (num(1)/lat:norm()) * lat
      else
        if n(2) > [1/math.sqrt(2)] then
          var a   = n(1)*n(1) + n(2)*n(2)
          fdir    = (num(1)/C.sqrt(a)) * v3(0,-n(2), n(1))
        else
          var a   = n(0)*n(0) + n(1)*n(1)
          fdir    = (num(1)/C.sqrt(a)) * v3(-n(1), n(0), 0)
        end
      end
    in fdir end
  end)

  PGS_State.methods.JACOBIAN_Norm = macro(function(pgs, ci, k, b_lin, b_ang)
    return quote
      var b0      = pgs.b0[ci]
      var b1      = pgs.b1[ci]
      var v0, v1  = b_lin[b0],    b_lin[b1]
      var w0, w1  = b_ang[b0],    b_ang[b1]

      var n       = pgs.basis[ci].norm
      var r0      = pgs.pts[ci].d[k].rel0
      var r1      = pgs.pts[ci].d[k].rel1

      var nwr0    = n:dot( w0:cross(r0) )
      var nwr1    = n:dot( w1:cross(r1) )
      var LM      = n:dot(v1) + nwr1 - n:dot(v0) - nwr0
    in LM end
  end)

  PGS_State.methods.JACOBIAN_Fric = macro(function(pgs, ci, k, b_lin, b_ang)
    return quote
      var b0      = pgs.b0[ci]
      var b1      = pgs.b1[ci]
      var v0, v1  = b_lin[b0],    b_lin[b1]
      var w0, w1  = b_ang[b0],    b_ang[b1]

      var f       = pgs.friction_dir[4*ci+k]
      var r0      = pgs.pts[ci].d[k].rel0
      var r1      = pgs.pts[ci].d[k].rel1

      var fwr0    = f:dot( w0:cross(r0) )
      var fwr1    = f:dot( w1:cross(r1) )
      var LM      = f:dot(v1) + fwr1 - f:dot(v0) - fwr0
    in LM end
  end)

  PGS_State.methods.JACOBIAN_T_Norm = macro(function(pgs, ci, k, lm)
    return quote
      var b0      = pgs.b0[ci]
      var b1      = pgs.b1[ci]

      var n       = pgs.basis[ci].norm
      var r0      = pgs.pts[ci].d[k].rel0
      var r1      = pgs.pts[ci].d[k].rel1

      var nLM     = lm * n
      var v0, w0  = -nLM, -r0:cross(nLM)
      var v1, w1  = nLM, r1:cross(nLM)
    in v0,w0,v1,w1 end
  end)

  PGS_State.methods.JACOBIAN_T_Fric = macro(function(pgs, ci, k, lm)
    return quote
      var b0      = pgs.b0[ci]
      var b1      = pgs.b1[ci]
      
      var f       = pgs.friction_dir[4*ci+k]
      var r0      = pgs.pts[ci].d[k].rel0
      var r1      = pgs.pts[ci].d[k].rel1

      var fLM     = lm * f
      var v0, w0  = -fLM, -r0:cross(fLM)
      var v1, w1  = fLM, r1:cross(fLM)
    in v0,w0,v1,w1 end
  end)

  PGS_State.methods.CacheJMJDiagonal = macro(function(pgs, ci)
    return quote
      var b0, b1      = pgs.b0[ci],           pgs.b1[ci]
      var I0, I1      = pgs.inv_inertia[b0],  pgs.inv_inertia[b1]
      var n           = pgs.basis[ci].norm

      var lin_diag    = pgs.inv_mass[b0] + pgs.inv_mass[b1]

      var n_pts       = pgs.n_pts[ci]
      var d_n     : num[4]
      var inv_d_n : num[4]
      var d_f     : num[4]
      var inv_d_f : num[4]
      for k=0,n_pts do
        var f         = pgs.friction_dir[4*ci+k]
        var r0        = pgs.pts[ci].d[k].rel0
        var r1        = pgs.pts[ci].d[k].rel1

        var r0n, r1n  = r0:cross(n),  r1:cross(n)
        var r0f, r1f  = r0:cross(f),  r1:cross(f)
        var DN        = lin_diag + r0n:dot( I0 * r0n ) + r1n:dot( I1 * r1n )
        var DF        = lin_diag + r0f:dot( I0 * r0f ) + r1f:dot( I1 * r1f )
        d_n[k]        = DN
        d_f[k]        = DF
        if DN == num(0) then  inv_d_n[k] = 0
                        else  inv_d_n[k] = num(1)/DN  end
        if DF == num(0) then  inv_d_f[k] = 0
                        else  inv_d_f[k] = num(1)/DF  end
      end
    in d_n, inv_d_n, d_f, inv_d_f end
  end)

  PGS_State.methods.integrateVelocities = macro(
  function(pgs, b, linvel, angvel, force, torque)
    return quote
      var v         = linvel[b] + timestep * pgs.inv_mass[b]    * force[b]
      var w         = angvel[b] + timestep * pgs.inv_inertia[b] * torque[b]
      -- TODO: Add a Gyroscopic force
    in v,w end
  end)
  PGS_State.methods.integratePositions = macro(
  function(pgs, b, pos, rot, v, w)
    return quote
      pos[b]        = pos[b] + timestep * v
      rot[b]        = rot[b]:integrateByAngVel( num(0.5*timestep)*w )
    end
  end)

  terra PGS_State:zero_push_delta()
    for b=0,self.n_boxes do
      self.push[b]      = v3(0,0,0)
      self.turn[b]      = v3(0,0,0)
      self.d_linvel[b]  = v3(0,0,0)
      self.d_angvel[b]  = v3(0,0,0)
    end
  end

  --[[
  Notes on system to solve:
    for matrix A, A^ is inverse and A' is transpose
    for quantity X, X0 is original val, X1 is new val
    - M : mat     mass
    - V : vec     velocity
    - J : mat     constraint jacobian
    - L : vec     Lagrange multipliers
    - F : vec     external force vector
    - dt : num    timestep length
    - b : num     penetration_correction_factor
    - Z = JV
    
  equations of motion:
    M(dV/dt) = J'L + F
           Z = JV

  use approximation (dV/dt) = (V1 - V0)/dt
    M (V1 - V0)  =  dt (J'L + F)
    V1 = V0 + dt M^ (J'L + F)
    Z = JV1 = JV0 + dt J M^ (J'L + F)
    Let
      A = J M^ J'
    In
      AL = Z/dt - J ( V0/dt - M^ F )
    which is the linear system we will attempt to solve
    Let
      RHS = Z/dt - J ( V0/dt - M^ F )
      A   = J M^ J'

  Projected Gauss Seidel Algorithm: (solves (J M^ J') L - RHS = 0)
    L   = L0          -- init lagrange multipliers
    F   = (M^ J') L   -- use force/torque as tmp
    D   = DIAGONAL[A]
    for iterations do
      for c in contacts do
        b0    = b0[c]
        b1    = b1[c]
        dL    = ( RHS[c] - J[c](F(b0),F(b1)) ) / D[c]
        Lp    = L[c]
        L[c]  = Proj( Lp + dL )
        dL    = L[c] - Lp
        F[b0] += dL * (M^ J')[c,b0]
        F[b1] += dL * (M^ J')[c,b1]
      end
    end

  --]]

  local pgs_loop = macro(function(
    pgs,
    JACOBIAN, JACOBIAN_T,
    linvel, angvel, mult, rhs,
    inv_diag, use_friction, in_push, max_err
  )
    use_friction  = use_friction:asvalue()
    in_push       = in_push:asvalue()
    JACOBIAN      = JACOBIAN:asvalue()
    JACOBIAN_T    = JACOBIAN_T:asvalue()
    assert(type(use_friction) == 'boolean')
    assert(type(in_push) == 'boolean')
    assert(type(JACOBIAN)     == 'string')
    assert(type(JACOBIAN_T)   == 'string')

    return quote
      for c=0,pgs.n_c_alloc do if pgs.is_live[c] then
        var b0,b1       = pgs.b0[c], pgs.b1[c]

        -- L0   = L[c]
        -- L[c] = Proj( L0 + ((RHS - J F)[c] / D[c]) )
        -- dL   = L[c] - L0
        for k=0,pgs.n_pts[c] do if (not in_push) or rhs[4*c+k] ~= num(0) then
          var norm_impls    = num(1)
          escape if use_friction then emit quote
            norm_impls      = pgs.l_mult[4*c+k]
          end end end
          -- only modify friction when we have a normal force
          if true then--norm_impls > num(0) then
            var J             = pgs:[JACOBIAN]( c, k, linvel, angvel )
            var tmp           = rhs[4*c+k] - inv_diag[4*c+k]*J
            var L0            = mult[4*c+k]
            tmp               = L0 + tmp
            -- Project
            escape if use_friction then emit quote
              var lim         = friction * norm_impls
              tmp             = clampf( tmp, -lim, lim )
            end else emit quote
              --C.printf(" LOWER LIM [%d] %f %f\n", c, L0, tmp )
              tmp             = maxf( tmp, 0 )
            end end end
            mult[4*c+k]       = tmp
            var dL            = tmp - L0
            max_err           = maxf(max_err, C.fabs(dL))

            -- F[b0] += dL * (M^ J')[c,b0]
            -- F[b1] += dL * (M^ J')[c,b1]
            var v0,w0,v1,w1   = pgs:[JACOBIAN_T]( c, k, dL )
            linvel[b0]        = linvel[b0] + pgs.inv_mass[b0]    * v0
            angvel[b0]        = angvel[b0] + pgs.inv_inertia[b0] * w0
            linvel[b1]        = linvel[b1] + pgs.inv_mass[b1]    * v1
            angvel[b1]        = angvel[b1] + pgs.inv_inertia[b1] * w1
          end
        end end
      end end
    end
  end)

  terra PGS_State:runPGS()
    var pgs         = @self

    -- form RHS and setup other values
    pgs:zero_push_delta()
    for c=0,pgs.n_c_alloc do if self.is_live[c] then
      -- Set the Friction Directions
      for k=0,pgs.n_pts[c] do
        pgs.friction_dir[4*c+k] = pgs:ComputeFrictionDir(c, k)
      end

      -- Cache Inverse Diagonal Data
      var D_N, I_N, D_F, I_F  = pgs:CacheJMJDiagonal(c)
      var f_mass              = num(1) / num(pgs.n_pts[c])
      for k=0,pgs.n_pts[c] do
        -- set diagonal
        pgs.inv_J[4*c+k]      = I_N[k]
        pgs.inv_J_fric[4*c+k] = I_F[k]
      end

      -- Compute the Right-Hand-Side and warmstart values...
      for k=0,pgs.n_pts[c] do
        var J             = pgs:JACOBIAN_Norm(c, k, pgs.linvel, pgs.angvel)
        var Jf            = pgs:JACOBIAN_Fric(c, k, pgs.linvel, pgs.angvel)
        -- velocity impulse is initialized to the goal of achieving 0 velocity
        var velImpulse    = -J * I_N[k]
        var posImpulse    = num(0)
        -- the required update to the position to resolve interpenetration
        -- can be handled either by updating the velocity (which we do in
        -- cases where the interpenetration is negative or very slight) or
        -- by updating the position directly. (in more severe conditions)
        var depth         = pgs.pts[c].d[k].depth
        if depth > 0 then
          posImpulse          = posImpulse
                              + inv_timestep * baumgarte * depth * I_N[k]
        else
          velImpulse          = velImpulse
                              + inv_timestep * depth * I_N[k]
        end
        if depth < split_pen_threshold then
          pgs.rhs[4*c+k]      = posImpulse + velImpulse
          pgs.rhsPos[4*c+k]   = num(0)
        else
          pgs.rhs[4*c+k]      = velImpulse
          pgs.rhsPos[4*c+k]   = posImpulse
        end
        -- set friction right-hand side
        pgs.rhs_fric[4*c+k]   = -Jf * I_F[k]
        --C.printf("     - c(k) %d(%d) - %f, %f p v %f %f invj %f J %f %f\n",
        --        c, k, depth, pgs.rhs[4*c+k], posImpulse, velImpulse,
        --        I_N[k], J, inv_timestep)

        -- intialize the linvel/angvel variables
        var b0,b1             = pgs.b0[c], pgs.b1[c]
        var im0, im1          = pgs.inv_mass[b0], pgs.inv_mass[b1]
        var iI0, iI1          = pgs.inv_inertia[b0], pgs.inv_inertia[b1]
        var LM                = pgs.l_mult[4*c+k]     * warmstart_factor
        var FM                = pgs.fric_mult[4*c+k]  * warmstart_factor
        var v0, w0, v1, w1    = pgs:JACOBIAN_T_Norm(c,k,LM)
        var vf0,wf0,vf1,wf1   = pgs:JACOBIAN_T_Fric(c,k,FM)
        pgs.d_linvel[b0]      = pgs.d_linvel[b0] + im0 * (v0 + vf0)
        pgs.d_angvel[b0]      = pgs.d_angvel[b0] + iI0 * (w0 + wf0)
        pgs.d_linvel[b1]      = pgs.d_linvel[b1] + im1 * (v1 + vf1)
        pgs.d_angvel[b1]      = pgs.d_angvel[b1] + iI1 * (w1 + wf1)
        --vf0, vf1, wf0, wf1 = im0*(v0+vf0), im1*(v1+vf1),
        --                     iI0*(w0+wf0), iI1*(w1+wf1)
        --C.printf('  vf0/1 %d:  %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n',
        --         c, vf0(0), vf0(1), vf0(2), vf1(0), vf1(1), vf1(2))
        --C.printf('  wf0/1 %d:  %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n',
        --         c, wf0(0), wf0(1), wf0(2), wf1(0), wf1(1), wf1(2))
        --C.printf("  LMFM : %7.3f %7.3f\n", LM, FM)
        --var r1                = pgs.pts[c].d[k].rel1
        --C.printf('    r1 :    %7.3f %7.3f %7.3f\n', r1(0), r1(1), r1(2))

        -- intialize the multiplier variables
        pgs.l_mult[4*c+k]     = LM
        pgs.fric_mult[4*c+k]  = FM
        pgs.pos_mult[4*c+k]   = num(0)
      end
    end end
    --for b=0,pgs.n_boxes do
    --  var v, w                = pgs.d_linvel[b], pgs.d_angvel[b]
    --  C.printf('  b %d:  %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n',
    --           b, v(0), v(1), v(2), w(0), w(1), w(2))
    --end

    var max_err     = num(0)
    var iter        = 0

    -- PGS Loop for Pushing
    while true do
      max_err = num(0)

      pgs_loop(pgs, 'JACOBIAN_Norm', 'JACOBIAN_T_Norm',
               pgs.push, pgs.turn, pgs.pos_mult, pgs.rhsPos,
               pgs.inv_J, false, true, max_err)

      iter = iter + 1
      if max_err < EPS then break end
      if iter >= max_iters then break end
    end

    -- PGS Loop
    iter            = 0
    while true do
      max_err = num(0)

      pgs_loop(pgs, 'JACOBIAN_Norm', 'JACOBIAN_T_Norm',
               pgs.d_linvel, pgs.d_angvel, pgs.l_mult, pgs.rhs,
               pgs.inv_J, false, false, max_err)

      pgs_loop(pgs, 'JACOBIAN_Fric', 'JACOBIAN_T_Fric',
               pgs.d_linvel, pgs.d_angvel, pgs.fric_mult, pgs.rhs_fric,
               pgs.inv_J_fric, true, false, max_err)

      iter = iter + 1
      if max_err < EPS then break end
      if iter >= max_iters then break end
    end

    -- independent error measurement
    --pgs:zero_forcetorque()
    --for c=0,pgs.n_c_alloc do if self.is_live[c] then
    --  var b0,b1       = pgs.b0[c], pgs.b1[c]
    --  var LM          = pgs.l_mult + 4*c
    --  var v0,w0,v1,w1 = pgs:JACOBIAN_T( c, LM )
    --  pgs.force[b0]   = pgs.force[b0]  + pgs.inv_mass[b0]    * v0
    --  pgs.torque[b0]  = pgs.torque[b0] + pgs.inv_inertia[b0] * w0
    --  pgs.force[b1]   = pgs.force[b1]  + pgs.inv_mass[b1]    * v1
    --  pgs.torque[b1]  = pgs.torque[b1] + pgs.inv_inertia[b1] * w1
    --end end
    --var max_err2, norm_err = num(0), num(0)
    --for c=0,pgs.n_c_alloc do if self.is_live[c] then
    --  var J           = pgs:JACOBIAN(c, pgs.force, pgs.torque)
    --  for k=0,pgs.n_pts[c] do
    --    var derr      = J[k] - pgs.rhs[4*c+k]
    --    var err       = derr:dot(derr)
    --    max_err2      = maxf(max_err2, err)
    --    norm_err      = norm_err + err
    --  end
    --end end

    C.printf("    solver iters:  %d\n", iter)
    C.printf("    max_err:  %20.10f\n", max_err)
    --C.printf("    max_err2: %20.10f\n", max_err2)
    --C.printf("    norm_err: %20.10f\n", norm_err)
  end

  terra PGS_State:do_timestep( sfunc : API.Store->{} )
    var pgs         = self -- MUST REMAIN A POINTER
                           -- because of unlock/relock store calls
    var starttime   = taketime()

    -- set gravity
    -- and save the current position/velocity state
    for b=0,pgs.n_boxes do
      pgs.pos_prev[b]     = pgs.pos[b]
      pgs.rot_prev[b]     = pgs.rot[b]
      pgs.linvel_prev[b]  = pgs.linvel[b]
      pgs.angvel_prev[b]  = pgs.angvel[b]
      var force           = v3(0, -pgs.mass[b]*gravity_acc, 0)
      var torque          = v3(0,0,0)
      var gyro            = pgs:gyroForce(b, timestep)
      --C.printf("GGG %f %f %f\n", gyro(0), gyro(1), gyro(2))
      --gyro = v3(0,0,0)
      pgs.force[b]        = force
      pgs.torque[b]       = torque + gyro
    end
    -- and cache the mass matrix, inverted
    pgs:cacheInvMassMatrix()

    -- DO COLLISION DETECTION!
    C.printf("  start collision\n")
    pgs:unlock_store()
    --C.printf("    unlocked\n")
    pgs.store:find_plank_iscts()
    pgs.store:PPContacts():sort()
    --C.printf("    locking\n")
    pgs:relock_store() -- resize constraint-value arrays
    var midtime     = taketime()
    C.printf("  end collision\n")
    if sfunc ~= nil then sfunc(pgs.store) end

    -- integrate forward velocities for the solver
    for b=0,pgs.n_boxes do
      var v, w = pgs:integrateVelocities( b, pgs.linvel, pgs.angvel,
                                             pgs.force,  pgs.torque )
      pgs.linvel[b]   = v
      pgs.angvel[b]   = w
    end

    -- Do the collision solve
    C.printf("  start solve\n")
    pgs:runPGS()

    -- now correct the predicted velocity and position using
    -- solved contact adjustments
    for b=0,pgs.n_boxes do
      var v                 = pgs.linvel[b] + pgs.d_linvel[b]
      var w                 = pgs.angvel[b] + pgs.d_angvel[b]
      pgs.linvel[b]         = v
      pgs.angvel[b]         = w

      var push, turn        = pgs.push[b], pgs.turn[b]
      --var dw                = pgs.d_angvel[b]
      --C.printf("     - b %d - %f %f %f  ;  %f %f %f\n",
      --         b, turn(0), turn(1), turn(2), dw(0), dw(1), dw(2))
      v                     = v + push
      w                     = w + num(0.1) * turn
      pgs:integratePositions(b, pgs.pos, pgs.rot, v, w)
    end

    var stoptime    = taketime()

    --pgs:print()
    var collide     = midtime  - starttime
    var solve       = stoptime - midtime
    var total       = stoptime - starttime
    C.printf(["  timings(ms) - total      %8.3f\n"..
              "                collision  %8.3f\n"..
              "                solver     %8.3f\n"],
              total*1e3, collide*1e3, solve*1e3)
    self.collide_time:acc(collide*1e3)
    self.solve_time:acc(solve*1e3)
    self.total_time:acc(total*1e3)
  end

  terra PGS_State:print_final_report()
    C.printf('\n*** Final Report ***\n')
    C.printf("  timings(ms) -  avg      min      max      | sum\n")
    C.printf("      total      %8.3f %8.3f %8.3f | %8.3f\n",
      self.total_time:avg(), self.total_time:min(),
      self.total_time:max(), self.total_time:sum() )
    C.printf("      collision  %8.3f %8.3f %8.3f | %8.3f\n",
      self.collide_time:avg(), self.collide_time:min(),
      self.collide_time:max(), self.collide_time:sum() )
    C.printf("      solver     %8.3f %8.3f %8.3f | %8.3f\n",
      self.solve_time:avg(), self.solve_time:min(),
      self.solve_time:max(), self.solve_time:sum() )
  end

  return PGS_State
end


------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
--

Exports.GeneratePGSSolver   = GeneratePGSSolver




