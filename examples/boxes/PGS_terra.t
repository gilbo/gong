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

  local vec3  = API.vec3:terratype()
  local quat  = API.quat:terratype()
  local mat3  = API.mat3:terratype()
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
  local baumgarte         = params.baumgarte or 0.2
  local beta_penetration  = constant(num, baumgarte * inv_timestep:asvalue())

  local friction_coefficient  = params.friction_coefficient or 0.30
  local gravity_acc           = params.gravity_acc or 9.8
  friction_coefficient    = constant(num, friction_coefficient)
  gravity_acc             = constant(num, gravity_acc)

  local max_iters         = params.max_iters or 50

  local EPS               = constant(num, 1e-5)

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  local struct PGS_State {
    -- scalar solver data
    --alpha_numerator : num
    --denominator     : num
    --beta            : num
    -- data sizes
    n_boxes         : int
    n_contacts      : int
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
    pos             : &vec3
    rot             : &quat
    linvel          : &vec3
    angvel          : &vec3
    force           : &vec3
    torque          : &vec3
    mass            : &num
    dims            : &vec3
    -- Contact-parallel 4x data
    rhs             : &vec3
    friction_limit  : &vec3
    inv_diag        : &mat3
    --
    l_mult          : &vec3
    -- Contact-parallel 1x data
    b0              : &uint
    b1              : &uint
    n_pts           : &uint
    pts             : &ContactT
    basis           : &ContactBase

    store           : API.Store
  }

  terra PGS_State:alloc( store : API.Store )
    self.store          = store

    var bs              = store:Planks()
    var cs              = store:PPContacts()
    var nB              = bs:getsize()
    var nC              = cs:getsize()
    self.n_boxes        = nB
    self.n_contacts     = nC

    -- box parallel additional
    escape for _,data in ipairs{
      {'inv_mass',    num },  {'inv_inertia', mat3},
      {'linvel_prev', vec3},  {'angvel_prev', vec3},
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
      'rhs', 'friction_limit',
    } do emit quote
      self.[name]       = [&vec3](C.malloc( nC * 4 * sizeof(vec3) ))
    end end end
    self.inv_diag       = [&mat3](C.malloc( nC * 4 * sizeof(mat3) ))

    -- contact-parallel 4x with cast from store
    self.l_mult         = [&vec3]( cs:l_mult():readwrite_lock() )

    -- contact parallel from store
    self.b0             = cs:p0():readwrite_lock()
    self.b1             = cs:p1():readwrite_lock()
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

    escape for _,name in ipairs{
      'n_pts', 'pts', 'basis', 'p0', 'p1', 'l_mult',
    } do emit quote
      cs:[name]():readwrite_unlock()
    end end end
  end
  terra PGS_State:relock_store()
    var bs              = self.store:Planks()
    assert( self.n_boxes == bs:getsize() )
    var cs              = self.store:PPContacts()
    var newsize         = cs:getsize()
    var oldsize         = self.n_contacts
    self.n_contacts     = newsize
    C.printf("    Found %d contacts\n", newsize)

    -- box parallel from store
    escape for _,name in ipairs{
      'pos', 'rot', 'linvel', 'angvel', 'force', 'torque', 'mass', 'dims',
    } do emit quote
      self.[name]       = bs:[name]():readwrite_lock()
    end end end

    -- contact parallel additional
    escape for _,name in ipairs{
      'rhs', 'friction_limit',
    } do emit quote
      self.[name] = [&vec3](C.realloc( self.[name], newsize*4*sizeof(vec3) ))
    end end end
    self.inv_diag = [&mat3](C.realloc(self.inv_diag, newsize*4*sizeof(mat3)))

    -- contact-parallel 4x with cast from store
    self.l_mult     = [&vec3]( cs:l_mult():readwrite_lock() )

    -- contact parallel from store
    self.b0         = cs:p0():readwrite_lock()
    self.b1         = cs:p1():readwrite_lock()
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
      'rhs', 'friction_limit', 'inv_diag',
    } do emit quote
      C.free(self.[name])
      self.[name] = nil
    end end end

    -- contact parallel from store
    cs:p0():readwrite_unlock()
    cs:p1():readwrite_unlock()
    escape for _,name in ipairs{
      'n_pts', 'pts', 'basis', 'l_mult',
    } do emit quote
      self.[name] = nil
      cs:[name]():readwrite_unlock()
    end end end
  end

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  terra PGS_State:print()
    C.printf("  BOXES       #%d\n", self.n_boxes)
    C.printf("           pos     -       -       ; vel     -       -       \n")
    C.printf("           force   -       -       ; torque  -       -       \n")
    for b=0,self.n_boxes do
      var p, v = self.pos[b], self.linvel[b]
      var f, t = self.force[b], self.torque[b]
    C.printf("    %4d:  %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n",
             b, p(0), p(1), p(2), v(0), v(1), v(2))
    C.printf("           %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f %7.3f\n",
             b, f(0), f(1), f(2), t(0), t(1), t(2))
    end
    C.printf("  CONTACTS    #%d\n", self.n_contacts)
    C.printf("              l_mult  -       -       ; rhs     -       -       ; depth\n")
    --C.printf("              invdiag -       -       ; -       -       -\n")
    --C.printf("              -       -       -       ; depth   -       \n")
    --C.printf("              force   -       -       ; torque  -       \n")
    for c=0,self.n_contacts do
    C.printf("    %4d: (%d)  %4d, %4d\n", c, self.n_pts[c],
                                          self.b0[c], self.b1[c])
      for k=0,self.n_pts[c] do
        var m, r = self.l_mult[4*c+k], self.rhs[4*c+k]
        var d    = self.pts[c].d[k].depth
        --var i    = self.inv_diag[4*c+k]
    C.printf("          %d : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f ; %7.3f\n",
             k, m(0), m(1), m(2), r(0), r(1), r(2), d)
    --C.printf("            : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n",
    --         k, i(0), i(1), i(2), 0,0,0)
    --C.printf("          %d : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n",
    --         k, 0,0,0, d(0), d(1), d(2))
      end
    end
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

  PGS_State.methods.JACOBIAN = macro(function(pgs, ci, k, b_lin, b_ang)
    if not b_ang then k, b_lin, b_ang = nil, k, b_lin end
    return quote
      var b0      = pgs.b0[ci]
      var b1      = pgs.b1[ci]
      var x0, x1  = pgs.pos[b0],  pgs.pos[b1]
      var v0, v1  = b_lin[b0],    b_lin[b1]
      var w0, w1  = b_ang[b0],    b_ang[b1]
      
      var n       = pgs.basis[ci].norm
      var f0      = pgs.basis[ci].friction_0
      var f1      = pgs.basis[ci].friction_1

      var n_pts   = pgs.n_pts[ci]
      var div     = num(1)--/n_pts
      --if n_pts == 0 then 
      var lm  : vec3[4]
      var lmk : vec3
      escape if k then emit quote
        var pt    = pgs.pts[ci].d[k].pt
        var r0    = pt-x0
        var r1    = pt-x1
        
        var wr0   = w0:cross(r0)
        var wr1   = w1:cross(r1)
        var d_acc = v1 + wr1 - v0 - wr0
        lmk       = div*v3(n:dot(d_acc), f0:dot(d_acc), f1:dot(d_acc))
      end else emit quote
        for k=0,n_pts do
          var pt    = pgs.pts[ci].d[k].pt
          var r0    = pt-x0
          var r1    = pt-x1
          
          var wr0   = w0:cross(r0)
          var wr1   = w1:cross(r1)
          var d_acc = v1 + wr1 - v0 - wr0
          lm[k]     = div*v3(n:dot(d_acc), f0:dot(d_acc), f1:dot(d_acc))
        end
      end end end
    in [(k and `lmk) or `lm] end
  end)

  PGS_State.methods.JACOBIAN_T = macro(function(pgs, ci, k, lm)
    if not lm then k, lm = nil, k end
    return quote
      var b0      = pgs.b0[ci]
      var b1      = pgs.b1[ci]
      var x0, x1  = pgs.pos[b0],  pgs.pos[b1]

      var n       = pgs.basis[ci].norm
      var f0      = pgs.basis[ci].friction_0
      var f1      = pgs.basis[ci].friction_1

      var v0, v1  = v3(0,0,0), v3(0,0,0)
      var w0, w1  = v3(0,0,0), v3(0,0,0)
      var n_pts   = pgs.n_pts[ci]
      var div     = num(1)--/n_pts
      escape if k then emit quote
        var pt    = pgs.pts[ci].d[k].pt
        var y     = div*lm
        var c_F   = y(0)*n + y(1)*f0 + y(2)*f1

        var r0    = pt-x0
        var r1    = pt-x1
        v0, w0    = -c_F, -r0:cross(c_F)
        v1, w1    = c_F, r1:cross(c_F)
      end else emit quote
        for k=0,n_pts do
          var pt    = pgs.pts[ci].d[k].pt
          var y     = div*lm[k]
          var c_F   = y(0)*n + y(1)*f0 + y(2)*f1

          var r0    = pt-x0
          var r1    = pt-x1
          v0, w0    = v0-c_F, w0-r0:cross(c_F)
          v1, w1    = v1+c_F, w1+r1:cross(c_F)
        end
      end end end
    in v0,w0,v1,w1 end
  end)

  PGS_State.methods.CacheJMJBlockDiagonal = macro(function(pgs, ci)
    return quote
      var b0          = pgs.b0[ci]
      var b1          = pgs.b1[ci]
      var n           = pgs.basis[ci].norm
      var f0          = pgs.basis[ci].friction_0
      var f1          = pgs.basis[ci].friction_1

      var lin_diag    = pgs.inv_mass[b0] + pgs.inv_mass[b1]
      -- REGULARIZATION
      --lin_diag        = lin_diag + 100*EPS
      var lin_mat     = m3(lin_diag, 0, 0,
                           0, lin_diag, 0,
                           0, 0, lin_diag)

      var x0, x1      = pgs.pos[b0],          pgs.pos[b1]
      var I0, I1      = pgs.inv_inertia[b0],  pgs.inv_inertia[b1]
      var n_pts       = pgs.n_pts[ci]
      var div         = num(1)--/n_pts
      var diag    : vec3[4]
      var invdiag : mat3[4]
      for k=0,n_pts do
        var pt        = pgs.pts[ci].d[k].pt
        var r0, r1    = x0-pt, x1-pt
        var r0n, r1n  = r0:cross(n),  r1:cross(n)
        var r0f0,r1f0 = r0:cross(f0), r1:cross(f0)
        var r0f1,r1f1 = r0:cross(f1), r1:cross(f1)
        var B0        = m3( r0n(0), r0f0(0), r0f1(0),
                            r0n(1), r0f0(1), r0f1(1),
                            r0n(2), r0f0(2), r0f1(2) )
        var B1        = m3( r1n(0), r1f0(0), r1f1(0),
                            r1n(1), r1f0(1), r1f1(1),
                            r1n(2), r1f0(2), r1f1(2) )
        var M0        = B0:transpose() * (I0 * B0)
        var M1        = B1:transpose() * (I1 * B1)
        var DM        = div*div*(M0 + M1 + lin_mat)
        diag[k]       = v3( DM(0,0), DM(1,1), DM(2,2) )
        invdiag[k]    = DM:invert()
      end
    in diag, invdiag end
  end)

  PGS_State.methods.integrateForward = macro(
  function(pgs, pos, rot, linvel, angvel, force, torque)
    return quote
      var dt          = timestep
      for b=0,pgs.n_boxes do
        var v         = linvel[b] + dt * pgs.inv_mass[b]    * force[b]
        var w         = angvel[b] + dt * pgs.inv_inertia[b] * torque[b]
        linvel[b]     = v
        angvel[b]     = w
        pos[b]        = pos[b] + dt * v
        rot[b]        = rot[b]:integrateByAngVel( num(0.5*dt)*w )
      end
    end
  end)

  terra PGS_State:zero_forcetorque()
    for b=0,self.n_boxes do
      self.force[b]   = v3(0,0,0)
      self.torque[b]  = v3(0,0,0)
    end
  end

  local v3_by_pt = macro(function(a,b)
    return `v3( a(0)*b(0), a(1)*b(1), a(2)*b(2) )
  end)

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

  terra PGS_State:runPGS()
    var pgs         = @self
    var iter        = 0

    -- Init RHS
    -- form: V0/dt - M^ F                       in tmp_vel
    for b=0,pgs.n_boxes do
      var linacc          = pgs.inv_mass[b]    * pgs.force[b]
      var angacc          = pgs.inv_inertia[b] * pgs.torque[b]
      pgs.force[b]        = inv_timestep * pgs.linvel[b] - linacc
      pgs.torque[b]       = inv_timestep * pgs.angvel[b] - angacc
    end
    -- form: Z/dt - J ( V0/dt - M^ F )          in rhs
    for c=0,pgs.n_contacts do
      var J               = pgs:JACOBIAN(c, pgs.force, pgs.torque)
      for k=0,pgs.n_pts[c] do
        var rhs           = -J[k]
        var depth         = pgs.pts[c].d[k].depth
        rhs(0)            = rhs(0) + inv_timestep * beta_penetration * depth
        pgs.rhs[4*c+k]    = rhs
      end
    end

    -- F = (M^ J')L
    -- also set inv_diag & friction_limit
    pgs:zero_forcetorque()
    for c=0,pgs.n_contacts do
      var b0,b1       = pgs.b0[c], pgs.b1[c]
      var LM          = pgs.l_mult + 4*c
      var v0,w0,v1,w1 = pgs:JACOBIAN_T( c, LM )
      pgs.force[b0]   = pgs.force[b0]  + pgs.inv_mass[b0]    * v0
      pgs.torque[b0]  = pgs.torque[b0] + pgs.inv_inertia[b0] * w0
      pgs.force[b1]   = pgs.force[b1]  + pgs.inv_mass[b1]    * v1
      pgs.torque[b1]  = pgs.torque[b1] + pgs.inv_inertia[b1] * w1

      var DIAG, INV   = pgs:CacheJMJBlockDiagonal(c)
      var f_mass      = num(1) / num(pgs.n_pts[c])
      for k=0,pgs.n_pts[c] do
        -- set diagonal and limits
        pgs.friction_limit[4*c+k] =
                    friction_coefficient * gravity_acc * f_mass * DIAG[k]
        pgs.inv_diag[4*c+k] = INV[k]
      end
    end

    --C.printf("START PGS\n")
    --pgs:print()

    -- PGS Loop
    var max_err = num(0)
    while true do
      max_err = num(0)
      for c=0,pgs.n_contacts do
        var b0,b1       = pgs.b0[c], pgs.b1[c]

        -- L0   = L[c]
        -- L[c] = Proj( L0 + ((RHS - J F)[c] / D[c]) )
        -- dL   = L[c] - L0
        for k=0,pgs.n_pts[c] do
          var J             = pgs:JACOBIAN(c, k, pgs.force, pgs.torque)
          -- REGULARIZATION
          --J                 = J + 100*v3(EPS,EPS,EPS)
          var tmp           = pgs.rhs[4*c+k] - J
          var L0            = pgs.l_mult[4*c+k]
          tmp               = L0 + pgs.inv_diag[4*c+k]*tmp
      --C.printf("  J %15.10f %15.10f %15.10f\n", J[k](0), J[k](1), J[k](2))
      --C.printf("  tmp %15.10f %15.10f %15.10f\n", tmp(0), tmp(1), tmp(2))
          -- Project
          var lim           = pgs.friction_limit[4*c+k]
          tmp               = v3(   maxf( tmp(0), 0 ),
                                  clampf( tmp(1), -lim(1), lim(1) ),
                                  clampf( tmp(2), -lim(2), lim(2) ) )
          pgs.l_mult[4*c+k] = tmp
          var dL            = tmp - L0
          max_err           = maxf(max_err, dL:dot(dL))

          -- F[b0] += dL * (M^ J')[c,b0]
          -- F[b1] += dL * (M^ J')[c,b1]
          var v0,w0,v1,w1   = pgs:JACOBIAN_T( c, k, dL )
          --C.printf(" b0 b1 %d %d\n", b0, b1)
          --C.printf("  w0 %15.10f %15.10f %15.10f\n", w0(0), w0(1), w0(2))
          --C.printf("  w1 %15.10f %15.10f %15.10f\n", w1(0), w1(1), w1(2))
          pgs.force[b0]     = pgs.force[b0]  + pgs.inv_mass[b0]    * v0
          pgs.torque[b0]    = pgs.torque[b0] + pgs.inv_inertia[b0] * w0
          pgs.force[b1]     = pgs.force[b1]  + pgs.inv_mass[b1]    * v1
          pgs.torque[b1]    = pgs.torque[b1] + pgs.inv_inertia[b1] * w1
        end

        --if iter < 3 then
        --  C.printf("====END PGS ITER %d c=%d\n", iter, c)
        --  pgs:print()
        --end
      end

      -- Experimental reset force/torque
      --pgs:zero_forcetorque()
      --for c=0,pgs.n_contacts do
      --  var b0,b1       = pgs.b0[c], pgs.b1[c]
      --  var LM          = pgs.l_mult + 4*c
      --  var v0,w0,v1,w1 = pgs:JACOBIAN_T( c, LM )
      --  pgs.force[b0]   = pgs.force[b0]  + pgs.inv_mass[b0]    * v0
      --  pgs.torque[b0]  = pgs.torque[b0] + pgs.inv_inertia[b0] * w0
      --  pgs.force[b1]   = pgs.force[b1]  + pgs.inv_mass[b1]    * v1
      --  pgs.torque[b1]  = pgs.torque[b1] + pgs.inv_inertia[b1] * w1
      --end

      iter = iter + 1
      if max_err < EPS*EPS then break end
      if iter >= max_iters then break end
    end

    -- independent error measurement
    --pgs:zero_forcetorque()
    --for c=0,pgs.n_contacts do
    --  var b0,b1       = pgs.b0[c], pgs.b1[c]
    --  var LM          = pgs.l_mult + 4*c
    --  var v0,w0,v1,w1 = pgs:JACOBIAN_T( c, LM )
    --  pgs.force[b0]   = pgs.force[b0]  + pgs.inv_mass[b0]    * v0
    --  pgs.torque[b0]  = pgs.torque[b0] + pgs.inv_inertia[b0] * w0
    --  pgs.force[b1]   = pgs.force[b1]  + pgs.inv_mass[b1]    * v1
    --  pgs.torque[b1]  = pgs.torque[b1] + pgs.inv_inertia[b1] * w1
    --end
    --var max_err2, norm_err = num(0), num(0)
    --for c=0,pgs.n_contacts do
    --  var J           = pgs:JACOBIAN(c, pgs.force, pgs.torque)
    --  for k=0,pgs.n_pts[c] do
    --    var derr      = J[k] - pgs.rhs[4*c+k]
    --    var err       = derr:dot(derr)
    --    max_err2      = maxf(max_err2, err)
    --    norm_err      = norm_err + err
    --  end
    --end

    --pgs:print()
    C.printf("    solver iters:  %d\n", iter)
    C.printf("    max_err:  %20.10f\n", max_err)
    --C.printf("    max_err2: %20.10f\n", max_err2)
    --C.printf("    norm_err: %20.10f\n", norm_err)
  end

  terra PGS_State:do_timestep()
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
      pgs.force[b]        = v3(0, -pgs.mass[b]*gravity_acc, 0)
      pgs.torque[b]       = v3(0,0,0)
    end
    -- and cache the mass matrix, inverted
    pgs:cacheInvMassMatrix()

    -- integrate forward
    pgs:integrateForward( pgs.pos,    pgs.rot,
                          pgs.linvel, pgs.angvel,
                          pgs.force,  pgs.torque )

    -- DO COLLISION DETECTION!
    pgs:unlock_store()
    pgs.store:find_plank_iscts()
    pgs:relock_store() -- resize constraint-value arrays
    var midtime     = taketime()

    -- Do the collision solve
    pgs:runPGS()

    -- unpack the result, and then integrate the result
    -- set gravity force,
    -- restore the old state
    for b=0,pgs.n_boxes do
      pgs.pos[b]          = pgs.pos_prev[b]
      pgs.rot[b]          = pgs.rot_prev[b]
      pgs.linvel[b]       = pgs.linvel_prev[b]
      pgs.angvel[b]       = pgs.angvel_prev[b]
      pgs.force[b]        = v3(0, -pgs.mass[b]*gravity_acc, 0)
      pgs.torque[b]       = v3(0,0,0)
    end
    -- accumulate the constraint forces
    for c=0,pgs.n_contacts do
      var b0,b1           = pgs.b0[c], pgs.b1[c]
      var LM              = pgs.l_mult + 4*c
      var v0,w0,v1,w1     = pgs:JACOBIAN_T(c,LM)
      pgs.force[b0]       = pgs.force[b0]  + v0
      pgs.torque[b0]      = pgs.torque[b0] + w0
      pgs.force[b1]       = pgs.force[b1]  + v1
      pgs.torque[b1]      = pgs.torque[b1] + w1
    end

    --pgs:print()

    pgs:integrateForward( pgs.pos,    pgs.rot,
                          pgs.linvel, pgs.angvel,
                          pgs.force,  pgs.torque )
    var stoptime    = taketime()

    var collide     = midtime  - starttime
    var solve       = stoptime - midtime
    var total       = stoptime - starttime
    C.printf(["  timings(ms) - total      %8.3f\n"..
              "                collision  %8.3f\n"..
              "                solver     %8.3f\n"],
              total*1e3, collide*1e3, solve*1e3)
  end

  return PGS_State
end


------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
--

Exports.GeneratePGSSolver   = GeneratePGSSolver




