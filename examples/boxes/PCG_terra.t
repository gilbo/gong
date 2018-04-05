import 'gong'
local G = gong.stdlib

local Exports = {}
package.loaded["PCG_terra"] = Exports


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
-- PCG Solver

local function GeneratePCGSolver(API, params)
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

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  local struct PCG_State {
    -- scalar solver data
    alpha_numerator : num
    denominator     : num
    beta            : num
    -- data sizes
    n_boxes         : int
    n_contacts      : int
    -- Boxes-parallel data
    tmp_linvel      : &vec3
    tmp_angvel      : &vec3
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
    D               : &vec3
    R               : &vec3
    P               : &vec3
    friction_limit  : &vec3
    diagonal        : &vec3
    inv_diag        : &vec3
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

  terra PCG_State:alloc( store : API.Store )
    self.store          = store

    var bs              = store:Planks()
    var cs              = store:PPContacts()
    var nB              = bs:getsize()
    var nC              = cs:getsize()
    self.n_boxes        = nB
    self.n_contacts     = nC

    -- box parallel additional
    escape for _,data in ipairs{
      {'tmp_linvel',  vec3},  {'tmp_angvel',  vec3},
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
      'rhs', 'D', 'R', 'P', 'friction_limit', 'diagonal', 'inv_diag',
    } do emit quote
      self.[name]       = [&vec3](C.malloc( nC * 4 * sizeof(vec3) ))
    end end end

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
  terra PCG_State:unlock_store()
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
  terra PCG_State:relock_store()
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
      'rhs', 'D', 'R', 'P', 'friction_limit', 'diagonal', 'inv_diag',
    } do emit quote
      self.[name] = [&vec3](C.realloc( self.[name], newsize*4*sizeof(vec3) ))
    end end end

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
  terra PCG_State:free()
    var bs              = self.store:Planks()
    var cs              = self.store:PPContacts()

    -- box parallel additional
    escape for _,name in ipairs{
      'tmp_linvel',  'tmp_angvel',  'inv_mass', 'inv_inertia',
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
      'rhs', 'D', 'R', 'P', 'friction_limit', 'diagonal', 'inv_diag',
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

  terra PCG_State:print()
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
    --C.printf("              -       -       -       ; depth   -       \n")
    --C.printf("              force   -       -       ; torque  -       \n")
    for c=0,self.n_contacts do
    C.printf("    %4d: (%d)  %4d, %4d\n", c, self.n_pts[c],
                                          self.b0[c], self.b1[c])
      for k=0,self.n_pts[c] do
        var m, r = self.l_mult[4*c+k], self.rhs[4*c+k]
        var d    = self.pts[c].d[k].depth
    C.printf("          %d : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f ; %7.3f\n",
             k, m(0), m(1), m(2), r(0), r(1), r(2), d)
    --C.printf("          %d : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n",
    --         k, 0,0,0, d(0), d(1), d(2))
      end
    end
  end
  terra PCG_State:print_liniter()
    C.printf("  CONTACTS    #%d\n", self.n_contacts)
    C.printf("              l_mult  -       -       ; P       -       -\n")
    C.printf("              D       -       -       ; R       -       -\n")
    for c=0,self.n_contacts do
     -- var p, v = self.pos[b], self.linvel[b]
      var f, t = self.force[0], self.torque[0]
    C.printf("    %4d: (%d)\n", c, self.n_pts[c])
      for k=0,self.n_pts[c] do
        var m, p = self.l_mult[4*c+k], self.P[4*c+k]
        var d, r = self.D[4*c+k], self.R[4*c+k]
    C.printf("          %d : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n",
             k, m(0), m(1), m(2), p(0), p(1), p(2))
    C.printf("            : %7.3f %7.3f %7.3f ; %7.3f %7.3f %7.3f\n",
             k, d(0), d(1), d(2), r(0), r(1), r(2))
      end
    end
  end

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  
  terra PCG_State:cacheInvMassMatrix()
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

  PCG_State.methods.JACOBIAN = macro(function(pcg, ci, b_lin, b_ang)
    return quote
      var b0      = pcg.b0[ci]
      var b1      = pcg.b1[ci]
      var x0, x1  = pcg.pos[b0],  pcg.pos[b1]
      var v0, v1  = b_lin[b0],    b_lin[b1]
      var w0, w1  = b_ang[b0],    b_ang[b1]
      
      var n       = pcg.basis[ci].norm
      var f0      = pcg.basis[ci].friction_0
      var f1      = pcg.basis[ci].friction_1

      var n_pts   = pcg.n_pts[ci]
      var lm : vec3[4]
      for k=0,n_pts do
        var pt    = pcg.pts[ci].d[k].pt
        var r0    = pt-x0
        var r1    = pt-x1
        
        var wr0   = w0:cross(r0)
        var wr1   = w1:cross(r1)
        var d_acc = v1 + wr1 - v0 - wr0
        lm[k]     = v3(n:dot(d_acc), f0:dot(d_acc), f1:dot(d_acc))
      end
    in lm end
  end)

  PCG_State.methods.JACOBIAN_T = macro(function(pcg, ci, lm)
    return quote
      var b0      = pcg.b0[ci]
      var b1      = pcg.b1[ci]
      var x0, x1  = pcg.pos[b0],  pcg.pos[b1]

      var n       = pcg.basis[ci].norm
      var f0      = pcg.basis[ci].friction_0
      var f1      = pcg.basis[ci].friction_1

      var v0, v1  = v3(0,0,0), v3(0,0,0)
      var w0, w1  = v3(0,0,0), v3(0,0,0)
      var n_pts   = pcg.n_pts[ci]
      for k=0,n_pts do
        var pt    = pcg.pts[ci].d[k].pt
        var y     = lm[4*ci+k]
        var c_F   = y(0)*n + y(1)*f0 + y(2)*f1

        var r0    = pt-x0
        var r1    = pt-x1
        v0, w0    = v0-c_F, w0-r0:cross(c_F)
        v1, w1    = v1+c_F, w1+r1:cross(c_F)
      end
    in v0,w0,v1,w1 end
  end)

  PCG_State.methods.CacheJMJdiagonal = macro(function(pcg, ci)
    return quote
      var b0          = pcg.b0[ci]
      var b1          = pcg.b1[ci]
      var n           = pcg.basis[ci].norm
      var f0          = pcg.basis[ci].friction_0
      var f1          = pcg.basis[ci].friction_1

      var lin_diag    = pcg.inv_mass[b0] + pcg.inv_mass[b1]

      var x0, x1      = pcg.pos[b0],          pcg.pos[b1]
      var I0, I1      = pcg.inv_inertia[b0],  pcg.inv_inertia[b1]
      var n_pts       = pcg.n_pts[ci]
      var diag : vec3[4]
      for k=0,n_pts do
        var pt        = pcg.pts[ci].d[k].pt
        var r0, r1    = x0-pt, x1-pt
        var r0n, r1n  = r0:cross(n),  r1:cross(n)
        var r0f0,r1f0 = r0:cross(f0), r1:cross(f0)
        var r0f1,r1f1 = r0:cross(f1), r1:cross(f1)
        diag[k] = v3( lin_diag +  r0n:dot(I0*r0n)  +  r1n:dot(I1*r1n),
                      lin_diag + r0f0:dot(I0*r0f0) + r1f0:dot(I1*r1f0),
                      lin_diag + r0f1:dot(I0*r0f1) + r1f1:dot(I1*r1f1) )
      end
    in diag end
  end)

  PCG_State.methods.integrateForward = macro(
  function(pcg, pos, rot, linvel, angvel, force, torque)
    return quote
      var dt          = timestep
      for b=0,pcg.n_boxes do
        var v         = linvel[b] + dt * pcg.inv_mass[b]    * force[b]
        var w         = angvel[b] + dt * pcg.inv_inertia[b] * torque[b]
        linvel[b]     = v
        angvel[b]     = w
        pos[b]        = pos[b] + dt * v
        rot[b]        = rot[b]:integrateByAngVel( num(0.5*dt)*w )
      end
    end
  end)

  terra PCG_State:zero_tmpvel()
    for b=0,self.n_boxes do
      self.tmp_linvel[b]  = v3(0,0,0)
      self.tmp_angvel[b]  = v3(0,0,0)
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

  PCG Algorithm:  (solves A L - RHS = 0...)
    D   = RHS - A L0
    R   = RHS - A L0
    for i=0,... (until convergence) do
      P   = A D
      d   = <D,P>
      a   = <D,R> / d
      L   = Proj( L + a D )
      R   = RHS - A L
      b   = <R,P> / d
      D   = ProjBy(L, R - b * D)
    end
  --]]

  terra PCG_State:runPCG()
    var pcg         = @self
    var EPS         = num(1e-5 * pcg.n_contacts)
    var iter        = 0

    -- Init RHS
    -- form: V0/dt - M^ F                       in tmp_vel
    for b=0,pcg.n_boxes do
      var linacc          = pcg.inv_mass[b]    * pcg.force[b]
      var angacc          = pcg.inv_inertia[b] * pcg.torque[b]
      pcg.tmp_linvel[b]   = inv_timestep * pcg.linvel[b] - linacc
      pcg.tmp_angvel[b]   = inv_timestep * pcg.angvel[b] - angacc
    end
    -- form: Z/dt - J ( V0/dt - M^ F )          in rhs
    for c=0,pcg.n_contacts do
      var J               = pcg:JACOBIAN(c, pcg.tmp_linvel, pcg.tmp_angvel)
      for k=0,pcg.n_pts[c] do
        var rhs           = -J[k]
        var depth         = pcg.pts[c].d[k].depth
        rhs(0)            = rhs(0) + inv_timestep * beta_penetration * depth
        pcg.rhs[4*c+k]    = rhs
      end
    end

    -- Zero out tmp
    pcg:zero_tmpvel()
    -- R,D = RHS - A L0
    -- a   = <D,R>
    -- set friction_limit, diagonal, inv_diagonal
    for c=0,pcg.n_contacts do
      var b0,b1           = pcg.b0[c], pcg.b1[c]
      var v0,w0,v1,w1     = pcg:JACOBIAN_T(c, pcg.l_mult)
      pcg.tmp_linvel[b0]  = pcg.tmp_linvel[b0] + pcg.inv_mass[b0]    * v0
      pcg.tmp_angvel[b0]  = pcg.tmp_angvel[b0] + pcg.inv_inertia[b0] * w0
      pcg.tmp_linvel[b1]  = pcg.tmp_linvel[b1] + pcg.inv_mass[b1]    * v1
      pcg.tmp_angvel[b1]  = pcg.tmp_angvel[b1] + pcg.inv_inertia[b1] * w1
    end
    pcg.alpha_numerator   = 0
    for c=0,pcg.n_contacts do
      var J               = pcg:JACOBIAN(c, pcg.tmp_linvel, pcg.tmp_angvel)
      var DIAG            = pcg:CacheJMJdiagonal(c)
      var f_mass          = num(1) / num(pcg.n_pts[c])
      for k=0,pcg.n_pts[c] do
        var init            = pcg.rhs[4*c+k] - J[k]
        pcg.R[4*c+k]        = init
        pcg.D[4*c+k]        = init
        -- set diagonal and limits
        pcg.diagonal[4*c+k] = DIAG[k]
        pcg.friction_limit[4*c+k] =
                    friction_coefficient * gravity_acc * f_mass * DIAG[k]
        for d=0,3 do
          var n = DIAG[k](d)
          if n < num(1e-5) then n = num(1e-5) end
          pcg.inv_diag[4*c+k](d) = num(1)/n
        end
        -- also computing <D,R> here...
        var pre_D           = v3_by_pt(init, pcg.inv_diag[4*c+k])
        pcg.alpha_numerator = pcg.alpha_numerator + init:dot(pre_D)
      end
    end

    -- P = A D  (first half)
    pcg:zero_tmpvel()
    for c=0,pcg.n_contacts do
      var b0,b1           = pcg.b0[c], pcg.b1[c]
      var v0,w0,v1,w1     = pcg:JACOBIAN_T(c, pcg.D)
      pcg.tmp_linvel[b0]  = pcg.tmp_linvel[b0] + pcg.inv_mass[b0]    * v0
      pcg.tmp_angvel[b0]  = pcg.tmp_angvel[b0] + pcg.inv_inertia[b0] * w0
      pcg.tmp_linvel[b1]  = pcg.tmp_linvel[b1] + pcg.inv_mass[b1]    * v1
      pcg.tmp_angvel[b1]  = pcg.tmp_angvel[b1] + pcg.inv_inertia[b1] * w1
    end

    --C.printf('PRE LOOP\n')
    --pcg:print()

    -- PCG Loop
    while true do
      pcg.denominator = 0

      -- P = A D  (finish)
      -- d = <D,P>
      for c=0,pcg.n_contacts do
        var J             = pcg:JACOBIAN(c, pcg.tmp_linvel, pcg.tmp_angvel)
        for k=0,pcg.n_pts[c] do
          var P           = v3_by_pt(J[k], pcg.inv_diag[4*c+k])
          pcg.P[4*c+k]    = P
          pcg.denominator = pcg.denominator + P:dot(pcg.D[4*c+k])
        end
      end

      --C.printf("  ITER %2d: a/d %7.3f/%7.3f\n", iter,
      --         pcg.alpha_numerator, pcg.denominator)
      --pcg:print_liniter()
      -- terminate?
      if iter >= max_iters then break end
      if pcg.denominator < EPS*EPS then break end
      var alpha = pcg.alpha_numerator / pcg.denominator

      -- L = Proj( L + a D )
      -- R = RHS - A L (first half)
      pcg:zero_tmpvel()
      for c=0,pcg.n_contacts do
        for k=0,pcg.n_pts[c] do
          var L             = v3_by_pt(pcg.D[4*c+k], pcg.inv_diag[4*c+k])
          L                 = pcg.l_mult[4*c+k] + alpha * L
          -- project L
          var lim           = pcg.friction_limit[4*c+k]
          pcg.l_mult[4*c+k] = v3(   maxf( L(0), 0 ),
                                  clampf( L(1), -lim(1), lim(1) ),
                                  clampf( L(2), -lim(2), lim(2) ) )
        end

        -- Jacobian Transpose...
        var b0,b1           = pcg.b0[c], pcg.b1[c]
        var v0,w0,v1,w1     = pcg:JACOBIAN_T(c, pcg.l_mult)
        pcg.tmp_linvel[b0]  = pcg.tmp_linvel[b0] + pcg.inv_mass[b0]    * v0
        pcg.tmp_angvel[b0]  = pcg.tmp_angvel[b0] + pcg.inv_inertia[b0] * w0
        pcg.tmp_linvel[b1]  = pcg.tmp_linvel[b1] + pcg.inv_mass[b1]    * v1
        pcg.tmp_angvel[b1]  = pcg.tmp_angvel[b1] + pcg.inv_inertia[b1] * w1
      end

      -- R = RHS - A L (finish)
      -- b = <R,P> / d
      pcg.beta = 0
      for c=0,pcg.n_contacts do
        var J               = pcg:JACOBIAN(c, pcg.tmp_linvel, pcg.tmp_angvel)
        var denom           = pcg.denominator
        for k=0,pcg.n_pts[c] do
          var R             = pcg.rhs[4*c+k] - J[k]
          pcg.R[4*c+k]      = R
          pcg.beta          = pcg.beta + (R:dot(pcg.P[4*c+k]) / denom)
        end
      end

      -- D = ProjBy(L, R - b * D)
      -- P = A D  (first half)
      -- a = <D,R>
      pcg.alpha_numerator = 0
      pcg:zero_tmpvel()
      for c=0,pcg.n_contacts do
        for k=0,pcg.n_pts[c] do
          var L               = pcg.l_mult[4*c+k]
          var Ld              = pcg.R[4*c+k] - pcg.beta * pcg.D[4*c+k]
          -- project Ld
          var lim             = pcg.friction_limit[4*c+k]
          if L(0) == num(0) then Ld(0) = maxf( Ld(0), 0 ) end
          if     L(1) == -lim(1) then Ld(1) = maxf( Ld(1), 0 )
          elseif L(1) ==  lim(1) then Ld(1) = minf( Ld(1), 0 ) end
          if     L(2) == -lim(2) then Ld(2) = maxf( Ld(2), 0 )
          elseif L(2) ==  lim(2) then Ld(2) = minf( Ld(2), 0 ) end
          pcg.D[4*c+k]        = Ld
          -- also computing <D,R> here...
          var pre_Ld          = v3_by_pt(Ld, pcg.inv_diag[4*c+k])
          pcg.alpha_numerator = pcg.alpha_numerator + Ld:dot( pcg.R[4*c+k] )
        end

        var b0,b1           = pcg.b0[c], pcg.b1[c]
        var v0,w0,v1,w1     = pcg:JACOBIAN_T(c, pcg.D)
        pcg.tmp_linvel[b0]  = pcg.tmp_linvel[b0] + pcg.inv_mass[b0]    * v0
        pcg.tmp_angvel[b0]  = pcg.tmp_angvel[b0] + pcg.inv_inertia[b0] * w0
        pcg.tmp_linvel[b1]  = pcg.tmp_linvel[b1] + pcg.inv_mass[b1]    * v1
        pcg.tmp_angvel[b1]  = pcg.tmp_angvel[b1] + pcg.inv_inertia[b1] * w1
      end

      iter = iter + 1
    end
    C.printf("    solver iters:  %d\n", iter)
    C.printf("    debug PCG norm^2 res:  %20.10f\n", pcg.denominator)
  end

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  terra PCG_State:do_timestep()
    var pcg         = self -- MUST REMAIN A POINTER
                           -- because of unlock/relock store calls
    var starttime   = taketime()

    -- set gravity
    -- and save the current position/velocity state
    for b=0,pcg.n_boxes do
      pcg.pos_prev[b]     = pcg.pos[b]
      pcg.rot_prev[b]     = pcg.rot[b]
      pcg.linvel_prev[b]  = pcg.linvel[b]
      pcg.angvel_prev[b]  = pcg.angvel[b]
      pcg.force[b]        = v3(0, -pcg.mass[b]*gravity_acc, 0)
      pcg.torque[b]       = v3(0,0,0)
    end
    -- and cache the mass matrix, inverted
    pcg:cacheInvMassMatrix()

    -- integrate forward
    pcg:integrateForward( pcg.pos,    pcg.rot,
                          pcg.linvel, pcg.angvel,
                          pcg.force,  pcg.torque )

    -- DO COLLISION DETECTION!
    pcg:unlock_store()
    pcg.store:find_plank_iscts()
    pcg:relock_store() -- resize constraint-value arrays
    var midtime     = taketime()

    -- Do the collision solve
    pcg:runPCG()

    -- unpack the result, and then integrate the result
    -- set gravity force,
    -- restore the old state
    for b=0,pcg.n_boxes do
      pcg.pos[b]          = pcg.pos_prev[b]
      pcg.rot[b]          = pcg.rot_prev[b]
      pcg.linvel[b]       = pcg.linvel_prev[b]
      pcg.angvel[b]       = pcg.angvel_prev[b]
      pcg.force[b]        = v3(0, -pcg.mass[b]*gravity_acc, 0)
      pcg.torque[b]       = v3(0,0,0)
    end
    -- accumulate the constraint forces
    for c=0,pcg.n_contacts do
      var b0,b1           = pcg.b0[c], pcg.b1[c]
      var v0,w0,v1,w1     = pcg:JACOBIAN_T(c, pcg.l_mult)
      pcg.force[b0]       = pcg.force[b0]  + v0
      pcg.torque[b0]      = pcg.torque[b0] + w0
      pcg.force[b1]       = pcg.force[b1]  + v1
      pcg.torque[b1]      = pcg.torque[b1] + w1
    end

    pcg:print()

    pcg:integrateForward( pcg.pos,    pcg.rot,
                          pcg.linvel, pcg.angvel,
                          pcg.force,  pcg.torque )
    var stoptime    = taketime()

    var collide     = midtime  - starttime
    var solve       = stoptime - midtime
    var total       = stoptime - starttime
    C.printf(["  timings(ms) - total      %8.3f\n"..
              "                collision  %8.3f\n"..
              "                solver     %8.3f\n"],
              total*1e3, collide*1e3, solve*1e3)
  end

  return PCG_State
end


------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
--

Exports.GeneratePCGSolver   = GeneratePCGSolver


