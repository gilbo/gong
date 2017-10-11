include 'gong'

local G               = (gong.stdlib)


------------------------------------------------------------------------------
-- Declarative Specification absent loading data

local Planks          = G.NewTable('Planks')
Planks:NewField('pos', G.vec3f)
Planks:NewField('rot', G.vec4f)
local dims            = G.Const(G.vec3f, {2,1,2})
Planks:NewField('linvel', G.vec3f)
Planks:NewField('angvel', G.vec3f)
Planks:NewField('force',  G.vec3f)
Planks:NewField('torque', G.vec3f)
local mass            = G.Const(G.float, 1)
local invmass         = G.Const(G.float, 1.0/mass:getval() )
-- would be good to have an inverse inertial tensor...

local PPContacts      = G.NewTable('PPContacts', {
                          join_policy = 'transfer',
                          -- i.e. update pre-existing rows where possible
                        })
PPContacts:NewField('p0', Planks)
PPContacts:NewField('p1', Planks)
PPContacts:NewField('ptcontact', G.vec3f)
PPContacts:NewField('norm01', G.vec3f)
PPContacts:NewField('friction_0', G.vec3f)
PPContacts:NewField('friction_1', G.vec3f)
--PPContacts:NewField('friction_mass', G.float)
PPContacts:NewField('penetration_depth', G.float)

-- coefficients on norm, friction_0, friction_1
PPContacts:NewField('l_mult', G.vec3f)

local OBB_EPSILON         = G.Const(G.float, 1e-5)
local EDGE_FUDGE_FACTOR   = G.Const(G.float, 1.05)


local swap = G.macro(function(a, b)
  return gong quote
    var tmp = a; a = b; b = tmp
  end
end)

local clipQuad = G.Macro(function(Fa01, Fbvs, n_pts, clip_pts)
  return quote do

  end end
end)

local cullPoints = G.Macro(function( n_pts, A_pts, depths )
  return gong quote do

  end end
end)




local gong struct obbResult {
  is_isct   : bool
  n_pt      : G.int
  pt        : G.vec3f[4]
  norm      : G.vec3f[4]
  pen_depth : G.float[4]
}

local gong function NoObbResult() : obbResult
  return obbResult {
    is_isct = false
    n_pt    = 0
  }
end

local face_vert_obb = G.macro(function(
  dp, BB, R, code_off, axis, -- inputs
  min_depth, norm, inv_norm, case_code -- outputs
)
  return gong quote do
    var depth = G.fabs( dp[axis] ) - BB[axis]
    if depth > 0 then return NoObbResult() end
    if depth > min_depth then
      min_depth = depth
      norm      = mRow( R0, axis )
      inv_norm  = ( dp0[axis] < 0 )
      case_code = 0 + axis
    end
  end end
end)

local edge_edge_obb = G.macro(function(
  dirA__, absdirA__, absdirB__,
  dp0, hwidths, code_off, -- inputs
  min_depth, norm, inv_norm, case_code, pen_ratio -- outputs
)
  return gong quote do
    var dirA      = G.vec3f(dirA__)
    var absdirA   = G.vec3f(absdirA__)
    var absdirB   = G.vec3f(absdirB__)
    -- <dir,dp0>
    var c_depth   = vDot( dirA, dp0 )   -- centroid separation
    -- max |<dir,+-hwdiths>| = <|dir|,|hwidths|>  (dA already non-neg)
    var depth_0   = vDot( absdirA, hwidths )
    -- max |<dir*RBA,+-hwidths>| = <|dir*R10|,|hwidths|>
    var depth_1   = vDot( absdirB, hwidths )

    -- less than comparison
    var depth     = G.fabs(c_depth) - (depth_0 + depth_1)
    if depth > OBB_EPSILON then return NoObbResult() end -- early exit
    var norm_len  = G.float(G.sqrt( vDot( dirA, dirA ) ))
    if norm_len > OBB_EPSILON then
      var inv_len = G.float(1.0) / norm_len
      depth       = EDGE_FUDGE_FACTOR * inv_len * depth
      if depth > min_depth then
        min_depth = depth
        norm      = dirA * inv_len
        inv_norm  = (c_depth < 0)
        case_code = code_off + 6
        pen_ratio = depth_0 / (depth_0 + depth_1)
      end
    end
  end end
end)

local gong function obb_isct( p0 : Planks, p1 : Planks ) : obbResult
  var R0  = transpose( quat2mat(p0.rot) )
  var R1  = transpose( quat2mat(p1.rot) )

  -- compute relative linear and rotational offsets
  var dp  = p1.pos - p0.pos
  var dp0 = mvMult(R0,  dp)
  var dp1 = mvMult(R1, -dp)
  var R10 = mmMult(R0, transpose(R1))
  -- Compute component-wise absolute value of the rotational offset matrix
  var Q   : G.mat3f
  for ki=0,3 do for kj=0,3 do Q[ki][kj] = G.fabs(R10[ki][kj]) end end

  -- declare variables
  var norm      : G.vec3f
  var inv_norm  : G.bool
  var min_depth = G.float( -math.huge )
  var case_code : G.int32
  var pen_ratio : G.float

  var hwidths   = dims * 0.5f

  -- test box-axis tests, 3 axes for p0 and 3 axes for p1 = 6 tests
  var BB_1in0   = hwidths + mvMult( Q, hwidths )
  var BB_0in1   = hwidths + mvMult( transpose(Q), hwidths )
  -- p0-centered cases
  for axis = 0,3 do
    face_vert_obb( dp0, BB_1in0, R0, 0, axis,
                   min_depth, norm, inv_norm, case_code )
  end
  -- p1-centered cases
  for axis = 0,3 do
    face_vert_obb( dp1, BB_0in1, R1, 3, axis,
                   min_depth, norm, inv_norm, case_code )
  end

  -- guard against false edge-edge intersections with an epsilon
  for i=0,3 do for j=0,3 do Q[i][j] = Q[i][j] + OBB_EPSILON end end

  -- edge-edge tests 9 cases for each of 3x3 pairings of axes
  -- Everything here is computed in p0's reference frame
  var R = R10
  -- (e_x) x (R10 e_x)        |(e_x) x (R10 e_x)|     |(e_x R10) x (e_x)|
  edge_edge_obb(
    {0, -R[2][0], R[1][0]},  {0, Q[2][0], Q[1][0]},  {0, Q[0][2], Q[0][1]},
    dp0, hwidths, 0,      min_depth, norm, inv_norm, case_code, pen_ratio )
  --       x (R10 e_y)
  edge_edge_obb(
    {0, -R[2][1], R[1][1]},  {0, Q[2][1], Q[1][1]},  {Q[0][2], 0, Q[0][0]},
    dp0, hwidths, 1,      min_depth, norm, inv_norm, case_code, pen_ratio )
  --       x (R10 e_z)
  edge_edge_obb(
    {0, -R[2][2], R[1][2]},  {0, Q[2][2], Q[1][2]},  {Q[0][1], Q[0][0], 0},
    dp0, hwidths, 2,      min_depth, norm, inv_norm, case_code, pen_ratio )
  -- (e_y) x (R10 ...)
  edge_edge_obb(
    {R[2][0], 0, -R[0][0]},  {Q[2][0], 0, Q[0][0]},  {0, Q[1][2], Q[1][1]},
    dp0, hwidths, 3,      min_depth, norm, inv_norm, case_code, pen_ratio )
  edge_edge_obb(
    {R[2][1], 0, -R[0][1]},  {Q[2][1], 0, Q[0][1]},  {Q[1][2], 0, Q[1][0]},
    dp0, hwidths, 4,      min_depth, norm, inv_norm, case_code, pen_ratio )
  edge_edge_obb(
    {R[2][2], 0, -R[0][2]},  {Q[2][2], 0, Q[0][2]},  {Q[1][1], Q[1][0], 0},
    dp0, hwidths, 5,      min_depth, norm, inv_norm, case_code, pen_ratio )
  -- (e_z) x (R10 ...)
  edge_edge_obb(
    {-R[1][0], R[0][0], 0},  {Q[1][0], Q[0][0] 0},  {0, Q[2][2], Q[2][1]},
    dp0, hwidths, 6,      min_depth, norm, inv_norm, case_code, pen_ratio )
  edge_edge_obb(
    {-R[1][1], R[0][1], 0},  {Q[1][1], Q[0][1] 0},  {Q[2][2], 0, Q[2][0]},
    dp0, hwidths, 7,      min_depth, norm, inv_norm, case_code, pen_ratio )
  edge_edge_obb(
    {-R[1][2], R[0][2], 0},  {Q[1][2], Q[0][2] 0},  {Q[2][1], Q[2][0], 0},
    dp0, hwidths, 8,      min_depth, norm, inv_norm, case_code, pen_ratio )

  -- HERE.  We have an intersection as determined above
  var depth = -min_depth
  if inv_norm then norm = -norm end

  -- Edge-Edge collision
  if case_code >= 6 then
    -- get norms in different reference frames
    var normA       = norm
    var normB       = mvMult( transpose(R10), -norm )
    var world_norm  = mvMult( transpose(R0), norm )
    -- axes involved
    case_code       = case_code - 6
    var axisA       = case_code / 3
    var axisB       = case_code % 3
    var A0          = (axisA+1)%3
    var A1          = (axisA+2)%3
    var B0          = (axisB+1)%3
    var B1          = (axisB+2)%3
    -- sign of axes
    var sA0         = ( normA(A0) < 0 )? -1.f : 1.f
    var sA1         = ( normA(A1) < 0 )? -1.f : 1.f
    var sB0         = ( normB(B0) < 0 )? -1.f : 1.f
    var sB1         = ( normB(B1) < 0 )? -1.f : 1.f
    -- points on edges of A,B in local frames
    var mA          = G.vec3f({0,0,0})
    var mB          = G.vec3f({0,0,0})
    mA(A0)          = mA(A0) + sA0 * hwidths(A0)
    mA(A1)          = mA(A1) + sA1 * hwidths(A1)
    mB(B0)          = mB(B0) + sB0 * hwidths(B0)
    mB(B1)          = mB(B1) + sB1 * hwidths(B1)
    -- to world frames
    mA              = p0 + mvMult( transpose(R0) * mA )
    mB              = p1 + mvMult( transpose(R1) * mB )
    -- half-edge vectors in world frames
    var hA          = mRow( R0, axisA ) * hwidths(axisA)
    var hB          = mRow( R1, axisB ) * hwidths(axisB)
    var lhA2        = hwidths(axisA) * hwidths(axisA)
    var lhB2        = hwidths(axisB) * hwidths(axisB)
    -- So now, mA mB are midpoints of the two interpenetrating edges
    --         hA hB are the half-edge vector pointing along each edge
    -- So, the point of intersection lies at the solution of the system
    --    pa = mA + ta*hA
    --    pb = mB + tb*hB
    -- where pa=pb, or numerically |pa-pb| is minimized
    -- This minimization is of the quantity
    --    E = (mB-mA + tb*hB-ta*hA)^2
    --      = (mB-mA)^2 + 2<mB-mA,hB>*tb - 2<mB-mA,hA>*ta
    --                  - 2<hA,hB>*ta*tb + <hB,hB>*tb*tb + <hA,hA>*ta*ta
    var CA  = vDot(mB-mA, hA)
    var CB  = vDot(mB-mA, hB)
    var AB  = vDot(hA, hB)
    var AA  = lhA2
    var BB  = lhB2
    -- This energy is minimized by a solution to the system
    --    2*CA  =  2*AA*ta - 2*AB*tb
    --   -2*CB  = -2*AB*ta + 2*BB*tb
    -- Which by Cramer's rule is given by
    var denom = AA*BB - AB*AB
    var inv_d = G.float(1) / denom
    var ta    = ( BB*CA - AB*CB ) * inv_d
    var tb    = ( AB*CA - AA*CB ) * inv_d
    if G.fabs(inv_d) < OBB_EPSILON then ta = 0; tb = 0 end
    -- clamp values
    if ta < -1 then ta = -1 elseif ta > 1 then ta = 1 end
    if tb < -1 then tb = -1 elseif tb > 1 then tb = 1 end
    -- compute the final point as the average of these points
    var pt      = G.float(0.5) * (mA + hA*ta + mB + hB*tb)

    return obbResult { is_isct = true, n_pts = 0,
                       pt      = {pt,pt,pt,pt},
                       norm    = {world_norm,world_norm,world_norm,world_norm},
                       pen_depth = {depth,depth,depth,depth}
                     }

  -- face-body collision
  else
    if case_code >= 3 then -- flip plank 0 and 1 roles
      R10         = transpose(R10)
      swap(R0,R1)
      swap(dp0,dp1)
      swap(p0,p1)
    end
    -- compute axis codes
    var axisA     = case_code % 3
    var invA      = (dp0[axisA] < 0)? G.float(-1) : G.float(1)
    var Anorm_inB = mRow( R10, axisA )
    var axisB     = vMaxAbsAxis( Anorm_inB )
    var invB      = invA * ((Anorm_inB[axisB] > 0)? G.float(-1) : G.float(1))
    var A0        = (axisA+1)%3
    var A1        = (axisA+2)%3
    var B0        = (axisB+1)%3
    var B1        = (axisB+2)%3

    -- define a 2d coordinate system F spanned by A0,A1
    -- extract face rect bounds from A in F
    var Fa01      = G.vec2f({ hwidths[A0], hwidths[A1] })
    -- center of B's face in A's 3d coordinates
    var centerB   = dp0 + mCol( R10, axisB ) * ( hwidths[axisB] * invB )
    var cBF       = G.vec2f({ centerB[A0], centerB[A1] })
    -- axes of B's face in F
    var Fb0   = G.vec2f({ R10[A0][B0]*hwdiths[B0], R10[A1][B0]*hwdiths[B0] })
    var Fb1   = G.vec2f({ R10[A0][B1]*hwdiths[B1], R10[A1][B1]*hwdiths[B1] })
    -- 4 vertices of the face of B in F
    var Fbvs      : G.vec2f[4]
    Fbvs[0]       = cBF + Fb0 + Fb1
    Fbvs[1]       = cBF - Fb0 + Fb1
    Fbvs[2]       = cBF - Fb0 - Fb1
    Fbvs[3]       = cBF + Fb0 - Fb1

    -- compute the clipped points
    var n_pts     : G.int32
    var clip_pts  : G.vec2f[8]
    clipQuad(Fa01, Fbvs, n_pts, clip_pts)

    -- UNPROJECT the clipped points back to 3d
    var A_pts     : G.vec3f[8]
    var depths    : G.float[8]
    -- depth coordinates
    var c_depth   : G.float   = centerB[axisA]
    var Fbdepth0  : G.float   = R10[axisA][B0] * hwidths[B0]
    var Fbdepth1  : G.float   = R10[axisA][B1] * hwidths[B1]
    var Acutoff               = hwidths[axisA]
    -- solve  Fb * (a0,a1) = 2d_pt - cBF  to unproject
    var Fbdet     : G.float   = Fb0[0]*Fb1[1] - Fb0[1]*Fb1[0]
    var inv_Fbdet : G.float   = G.float(1) / Fbdet
    var w_i = 0
    for r_i = 0,n_pts do
      var pt      = clip_pts[r_i]
      var rhs     = pt - cBF
      var a0      = inv_Fbdet * ( rhs[0]*Fb1[1] - rhs[1]*Fb1[0] )
      var a1      = inv_Fbdet * ( rhs[1]*Fb0[0] - rhs[0]*Fb0[1] )
      var depth   = c_depth + a0*Fbdepth0 + a1*Fbdepth1
      -- depth check
      var pen_depth = Acutoff - invA*depth
      if pen_depth > -OBB_EPSILON then
        A_pts[w_i][axisA]   = depth
        A_pts[w_i][A0]      = pt[0]
        A_pts[w_i][A1]      = pt[1]
        depths[w_i]         = pen_depth
        w_i = w_i + 1
      end
    end
    n_pts = w_i

    -- Cull the set of points down
    if n_pts > 4 then
      -- Keep the deepest point regardless
      var idx         = 0
      var max_depth   = depths[0]
      for i=1,n_pts do
        if depths[i] > max_depth then idx = i; max_depth = depths[i] end
      end
      swap(depths[idx], depths[0])
      swap(A_pts[idx], A_pts[0])
      cullPoints( n_pts, A_pts, depths )
      n_pts = 4
    end

    -- Output
    if case_code >= 3 then -- unflip the data
      R10         = transpose(R10)
      swap(R0,R1)
      swap(dp0,dp1)
      swap(p0,p1)
    end
    var mass_per = mass/G.float(n_pts)
    for k=0,n_pts do
      var e       = G.vec3f({0,0,0})
      e[ vMinAbsAxis(norm) ] = 1
      var f0      = vNormalize( vCross(norm, e) )
      var f1      = vCross(norm, f0)
      emit { p0=p0, p1=p1, ptcontact=A_pts[k],
             norm01 = norm, friction_0 = f0, friction_1 = f1,
             penetration_depth = depths[k] }
        in PPContacts
    end
  end
end




local gong join find plank_iscts ()




local gong function is_isct_spheres( x : Spheres, y : Spheres ) : bool
  var d   = y.pos - x.pos
  var d2  = G.dot(d,d)
  return d2 < sphere_radius * sphere_radius
end

local gong join find_sphere_iscts ()
  x <- Spheres
  y <- Spheres
  var r = y.pos - x.pos
  where is_isct_spheres(x,y)
do
  emit { s0=x, s1=y, r=r } in SSContacts
end



------------------------------------------------------------------------------
-- Algorithmic / Functional Specfication

-- Grid approach
--local SHLib           = require 'spatial_hash'

local factor          = 2.001 -- 2 + epsilon ideally at a minimum
local cell_w          = G.Const(G.float, factor * sphere_radius:get())

local gong partition SpatialHash3f[n] {
  n   : G.size32

  init( sz : G.size32 )
    n = sz
  end

  -- argument becomes key
  use hashing( i : int32, j : int32, k : int32 ) : uint32
    --var Xmult = 0x3a99068f
    --var Ymult = 0xbe93625f
    --var Zmult = 0xe823bd4f
    var Xmult   = [G.int32](73856093)
    var Ymult   = [G.int32](19349663)
    var Zmult   = [G.int32](83492791)
    var hid     = (i*Xmult) ^ (j*Ymult) ^ (k*Zmult)
    return hid % n
  end

  generate( s : Spheres )
    var inv_w = 1.0f / cell_w
    var sp    = inv_w * s.pos
    var r     = { sphere_radius, sphere_radius, sphere_radius }
    var min   = G.floor(  sp - r  )
    var max   = G.ceil(   sp + r  )
    for i=min[0],max[0] do
      for j=min[1],max[1] do
        for k=min[2],max[2] do
          emit(i,j,k)
    end end end
  end
}

local n_bins          = 1e4


local hashIndex       =  G.Index()
                          :Partition( 'bins', SpatialHash3f )
                          :List( 'list' )
                          (Spheres)


local gong build construct_hashing( xs : G.Set(Spheres) ) : hashIndex
  xs  <- Partition(xs, SpatialHash3f(n_bins))
  x   <- List(xs)
  return x
end


local gong traversal self_hash_traversal( a : hashIndex, b : hashIndex )
  ( a == b @ bins ) => { expand(a,b) } -- scan the bins
  ( a == b @ list ) => {
    expand(a,b)           -- cross product of bins
  }
end

self_hash_traversal:DedupFilterBeforeAction()


-- set the indexing option, build & traversal schemes
find_sphere_iscts:UseAlgorithm {
  index_left    = hashIndex,
  index_right   = hashIndex,
  build_left    = construct_hashing,
  build_right   = construct_hashing,
  traversal     = self_hash_traversal,
}


------------------------------------------------------------------------------
-- Imperative Specfication / Schedule


-- schedule for a single CPU

local hbSchedule = construct_hashing:Schedule()
  ebSchedule:QueueBefore('bins')   :Priority(0)
    :CPU(0)
  -- allow the listing to be fused in

local travSchedule = self_hash_traversal:Schedule()
  travSchedule:QueueBefore('bins , bins')   :Priority(0)
    :CPU(0)

