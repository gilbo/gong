import 'gong'
local G = gong.stdlib

------------------------------------------------------------------------------
-- Sim Constants / Parameters

local OBB_EPSILON         = G.Constant(G.float, 1e-5)
local EDGE_FUDGE_FACTOR   = G.Constant(G.float, 1.05)


------------------------------------------------------------------------------
-- Support Functions / Defs

local num       = G.float
local vec2      = G.vector(num,2)
local vec3      = G.vector(num,3)
local mat3      = G.matrix(num,3,3)
local quat      = G.vector(num,4)

local swap = G.Macro(function(a, b)
  return gong quote a,b = b,a end
end)

local normed = G.Macro(function(a)
  return gong quote
    var b   = a
    var rn  = num(1f) / G.magnitude(b)
  in rn * b end
end)
local gong function vMaxAbsAxis( x : vec3 )
  var axis  = 0
  var val   = G.abs(x[0])
  for k=1,2 do
    var v   = G.abs(x[k])
    if v > val then axis = k; val = v end
  end
  return axis
end
local gong function vMinAbsAxis( x : vec3 )
  var axis  = 0
  var val   = G.abs(x[0])
  for k=1,2 do
    var v   = G.abs(x[k])
    if v < val then axis = k; val = v end
  end
  return axis
end

local gong function qId() return quat({0f,0f,0f,1f}) end
local gong function qVec( q : quat ) return {q[0],q[1],q[2]} end
local gong function qMult( lhs : quat, rhs : quat )
  --     q*p = ...
  -- (q*p).s = q.s * p.s - <q.v,p.v>
  -- (q*p).v = q.s * p.v + p.s * q.v + q.v x p.v
  return {
    lhs[0]*rhs[3] + lhs[3]*rhs[0] + lhs[1]*rhs[2] - lhs[2]*rhs[1],
    lhs[1]*rhs[3] + lhs[3]*rhs[1] + lhs[2]*rhs[0] - lhs[0]*rhs[2],
    lhs[2]*rhs[3] + lhs[3]*rhs[2] + lhs[0]*rhs[1] - lhs[1]*rhs[0],
    lhs[3]*rhs[3] - lhs[0]*rhs[0] - lhs[1]*rhs[1] - lhs[2]*rhs[2]
  }
end
local gong function qConj( q : quat )
  return { -q[0], -q[1], -q[2], q[3] }
end
local gong function qRot( q : quat, v : vec3 )
  -- = q * v * qConj(q)
  -- qv.s = 0 - <q.v,v>
  -- qv.v = 0 + q.s*v + q.v x v
  -- qvq*.v = qv.s * q*.v  +  q*.s * qv.v  +  qv.v x q*.v
  --        = <q.v,v> * q.v  +  q.s*q.s*v + q.s * (q.v x v)
  --                         -  (q.s*v + q.v x v) x q.v
  -- LET s = q.s, and u = q.v; furthermore, note that
  --        = <u,v>u + ssv + s(u x v) - (sv + u x v) x u
  --        = <u,v>u + ssv + s(u x v) - s(v x u) - (u x v) x u
  --        = <u,v>u + ssv + 2s(u x v) + u x (u x v)
  --        (NOTE identity:  u x (u x v) = u.v u - u.u v )
  --        = 2<u,v>u + (ss - <u,u>)v + 2s(u x v)
  var s = q[3]
  var u = qVec(q)
  return 2f*G.dot(u,v) * u           -- 1* 3* 2+ 3*       3+
       + (s*s - G.dot(u,u)) * v      -- 1* 1+ 3* 2+ 3*    3+
       + 2f*s * G.cross(u,v)         -- 1* 3* 6* 3+   (= 14+ 24* =38 ops)
end

local gong function qRotMat( q : quat ) : mat3
  var s = q[3]
  var q0 = q[0]
  var q1 = q[1]
  var q2 = q[2]
  -- Note that the literal is column-major
  return mat3({
  { 1f - 2f*(q1*q1 + q2*q2),     2f*(q0*q1 + q2*s),      2f*(q0*q2 - q1*s) },
  {      2f*(q0*q1 - q2*s), 1f - 2f*(q0*q0 + q2*q2),     2f*(q1*q2 + q0*s) },
  {      2f*(q0*q2 + q1*s),      2f*(q1*q2 - q0*s), 1f - 2f*(q0*q0 + q1*q1) }
  })
  -- 24* 12+ =36 ops to construct
end

local INTEGRATION_THRESHOLD = 0.25 * math.pi -- quarter of a circle
local gong function qIntegrateByAngVel( q : quat, w : vec3 )
  -- method uses exponential map between angvel vectors and quaternions
  var nw = G.magnitude(w)
  if 2.0*nw > INTEGRATION_THRESHOLD then
    nw = num(0.5 * INTEGRATION_THRESHOLD)
  end

  var axis : vec3 = vec3({0f,0f,0f})
  if nw < 0.001 then -- Taylor expansion near identity
    axis = w * (num(1) - num(1.0/6.0)*nw*nw)
  else
    axis = w * num( G.sin(nw) / nw )
  end
  var cosw = G.cos(nw)
  var expw = quat({ axis[0], axis[1], axis[2], cosw })
  -- normalize to prevent accumulating error drifting away from unit length
  return normed( qMult(expw, q) )
end


------------------------------------------------------------------------------
-- Schema

local Planks          = G.NewTable('Planks')
                         :NewField('pos',     vec3)
                         :NewField('rot',     quat)
                         :NewField('linvel',  vec3)
                         :NewField('angvel',  vec3)
                         :NewField('force',   vec3)
                         :NewField('torque',  vec3)
local dims            = G.Const(vec3, {2,1,2})
local mass            = G.Const(num,  1)
local invmass         = G.Const(num,  1.0/mass:getval() )
-- would be good to have an inverse inertial tensor...

local ContactT = G.record {
  { 'pt',                 vec3 },
  { 'norm',               vec3 },
  { 'depth',              num },
}
local ContactBase = G.record {
  { 'friction_0',         vec3 },
  { 'friction_1',         vec3 },
}

local PPContacts      = G.NewTable('PPContacts', {
                          --join_policy = 'transfer',
                          -- i.e. update pre-existing rows where possible
                        })
                         :NewField('p0',          Planks)
                         :NewField('p1',          Planks)
                         :NewField('n_pts',       G.uint32)
                         :NewField('pts',
                                   G.record({
                                      {'contact', ContactT},
                                      {'basis',   ContactBase},
                                   -- coefficients on norm, fric_0, fric_1
                                      {'l_mult',  vec3},
                                   })[4])
--                         :NewField('friction_mass', num[4])

local gong function NullContacts() : ContactT[4]
  return {
    { pt = vec3({0f,0f,0f}), norm = vec3({0f,0f,0f}), depth = num(0f) },
    { pt = vec3({0f,0f,0f}), norm = vec3({0f,0f,0f}), depth = num(0f) },
    { pt = vec3({0f,0f,0f}), norm = vec3({0f,0f,0f}), depth = num(0f) },
    { pt = vec3({0f,0f,0f}), norm = vec3({0f,0f,0f}), depth = num(0f) }
  }
end

local gong function NullBases() : ContactBase[4]
  return {
    { friction_0 = vec3({0f,0f,0f}), friction_1 = vec3({0f,0f,0f}) },
    { friction_0 = vec3({0f,0f,0f}), friction_1 = vec3({0f,0f,0f}) },
    { friction_0 = vec3({0f,0f,0f}), friction_1 = vec3({0f,0f,0f}) },
    { friction_0 = vec3({0f,0f,0f}), friction_1 = vec3({0f,0f,0f}) }
  }
end

------------------------------------------------------------------------------
-- Subroutines

local single_clip = G.Macro(function(
  rPts, nRead, wPts, nWrite,
  clip_sign, clip_i, clipby
)
  local clip_test = (clip_sign==1
    and G.Macro(function(v) return gong `v[clip_i] <=  clipby[clip_i] end))
    or  G.Macro(function(v) return gong `v[clip_i] >= -clipby[clip_i] end)
  local output = G.Macro(function(pt)
    return gong quote wPts[nWrite] = pt; nWrite = nWrite+1 end
  end)

  return gong quote do
    -- Setup for Clip Loop
    nWrite  = 0
    var prevPt = rPts[nRead-1]
    var prevIn = clip_test(prevPt)
    -- Clip Loop
    for k=0,nRead do
      var currPt  = rPts[k]
      var currIn  = clip_test(currPt)
      -- If we crossed the clip-line, then output the intersection point
      if currIn ~= prevIn then
        var c   = clip_sign * clipby[clip_i]
        var d   = currPt[clip_i] - prevPt[clip_i]
        var t   = c - prevPt[clip_i]
        if d*d < OBB_EPSILON*OBB_EPSILON then t = 0.0f
                                         else t = t/d end
        var pt = (1-t)*prevPt + t*currPt
        output(pt)
      end
      -- Regardless of intersection, now copy over the current point to
      -- the output array
      output(currPt)
      -- Adjust for next iteration
      G.assert(nWrite <= 8)
      prevPt  = currPt
      prevIn  = currIn
    end
  end end
end)
local gong function clipQuad( clipby : vec2, in_pts : vec2[4] )
  var clip_tmp  : vec2[8] = {{0f,0f},{0f,0f},{0f,0f},{0f,0f},
                             {0f,0f},{0f,0f},{0f,0f},{0f,0f}}
  var out_pts   : vec2[8] = {{0f,0f},{0f,0f},{0f,0f},{0f,0f},
                             {0f,0f},{0f,0f},{0f,0f},{0f,0f}}
  var nRead     = 4
  var nWrite    = 0

  single_clip(in_pts,   nRead, clip_tmp,  nWrite,  1, 0, clipby)
  nRead = nWrite
  single_clip(clip_tmp, nRead, out_pts,   nWrite,  1, 1, clipby)
  nRead = nWrite
  single_clip(out_pts,  nRead, clip_tmp,  nWrite, -1, 0, clipby)
  nRead = nWrite
  single_clip(clip_tmp, nRead, out_pts,   nWrite, -1, 1, clipby)
  
  return nWrite, out_pts
end

--[[
local clipQuad = G.Macro(function(clipby, in_pts, n_pts, out_pts)
  --  INPUT  clipby   : vec2f
  --         in_pts   : vec2f[4]
  --  OUTPUT n_pts    : int32
  --         out_pts  : vec2f[8]

  local single_clip = G.Macro(function(
    rPts, nRead, wPts, nWrite,
    clip_sign, clip_i
  )
    local clip_test = G.Macro(function(v)
      if clip_sign == 1 then
        return gong `v[clip_i] <= clipby[clip_i]
      else
        return gong `v[clip_i] >= -clipby[clip_i]
      end
    end)
    local output = G.Macro(function(pt)
      return gong quote do  wPts[nWrite] = pt;  nWrite = nWrite+1   end
    end)

    return gong quote do
      -- Setup for Clip Loop
      nWrite  = 0
      var prevPt = rPts[nRead-1]
      var prevIn = clip_test(prevPt)
      -- Clip Loop
      for k=0,nRead do
        var currPt  = rPts[k]
        var currIn  = clip_test(currPt)
        -- If we crossed the clip-line, then output the intersection point
        if currIn ~= prevIn then
          var c   = clip_sign * clipby[clip_i]
          var d   = currPt[clip_i] - prevPt[clip_i]
          var t   = c - prevPt[clip_i]
          if d*d < OBB_EPSILON*OBB_EPSILON then t = 0.0f
                                           else t = t/d end
          var pt = (1-t)*prevPt + t*currPt
          output(pt)
        end
        -- Regardless of intersection, now copy over the current point to
        -- the output array
        output(currPt)
        -- Adjust for next iteration
        G.assert(nWrite <= 8)
        prevPt  = currPt
        prevIn  = currIn
      end
    end end
  end)

  return gong quote do
    var clip_tmp  : G.vec2f[8]
    var nRead     = 4
    var nWrite    = 0

    single_clip(in_pts,   nRead, clip_tmp,  nWrite,  1, 0)
    nRead = nWrite
    single_clip(clip_tmp, nRead, out_pts,   nWrite,  1, 1)
    nRead = nWrite
    single_clip(out_pts,  nRead, clip_tmp,  nWrite, -1, 0)
    nRead = nWrite
    single_clip(clip_tmp, nRead, out_pts,   nWrite, -1, 1)
    
    n_pts = nWrite
  end end
end)
--]]

-- reduce the number of points down to 4
local cullPoints = G.Macro(function( n_pts, pts, depths )
  --  INPUT  n_pts    : int
  --  IN_OUT pts      : vec3[8]
  --         depths   : num[8]

  -- let d[i] = normalize(pts[i] - centroid)
  -- (See below)
  local compute_d = G.Macro(function(d)
    return gong quote
      var centroid = vec3({0,0,0})
      for k=0,n_pts do centroid += pts[k] end
      centroid *= (num(1.0f)/n_pts)

      for k=0,n_pts do
        var v   = pts[k] - centroid
        var n   = G.magnitude(v)
        d[k]    = (num(1) / ((n > OBB_EPSILON)? n else OBB_EPSILON)) * v
      end
    end
  end)

  -- Always keep point 0.
  -- Choose the 3 remaining points which minimize
  --  (1)   |< d[i], d[0] >|  (i.e. most orthogonal)
  --  (2)    < d[i], d[0] >   (i.e. most opposite)
  --  (3)   |< d[i], d[1] >|  (i.e. most opposite to most orthogonal)
  local case_logic = G.Macro(function(d, case)
    return gong quote do
      var i     = 0
      var best  = num(math.huge)
      for k=case,n_pts do
        var val : num = 0f
        if      case == 1 then  val = G.abs(+[i] d[0][i] * d[k][i])
        elseif  case == 2 then  val = +[i] d[0][i] * d[k][i]
                          else  val = +[i] d[1][i] * d[k][i] end
        -- yah
        if val < best then best = val; i = k end
      end
      swap(d[case],       d[i])
      swap(pts[case],     pts[i])
      swap(depths[case],  depths[i])
    end end
  end)

  return gong quote do
    var d   : G.vec3f[8] = {{0f,0f},{0f,0f},{0f,0f},{0f,0f},
                            {0f,0f},{0f,0f},{0f,0f},{0f,0f}}
    compute_d(d)
    case_logic(d, 1)
    case_logic(d, 2)
    case_logic(d, 3)
  end end
end)






local face_vert_obb = G.macro(function(
  dp, BB, R, code_off, axis, -- inputs
  min_depth, norm, inv_norm, case_code -- outputs
)
  return gong quote do
    var depth = G.abs( dp[axis] ) - BB[axis]
    if depth > 0 then return 0, NullContacts() end
    if depth > min_depth then
      min_depth = depth
      norm      = :[i] R0[axis,i]
      inv_norm  = ( dp[axis] < 0 )
      case_code = code_off + axis
    end
  end end
end)

local edge_edge_obb = G.macro(function(
  dirA__, absdirA__, absdirB__,
  dp0, hwidths, code_off, -- inputs
  min_depth, norm, inv_norm, case_code, pen_ratio -- outputs
)
  return gong quote do
    var dirA      = vec3(dirA__)
    var absdirA   = vec3(absdirA__)
    var absdirB   = vec3(absdirB__)
    -- <dir,dp0>
    var c_depth   = +[i] dirA[i] * dp0[i]   -- centroid separation
    -- max |<dir,+-hwidths>| = <|dir|,|hwidths|>  (dA already non-neg)
    var depth_0   = +[i] absdirA[i]*hwidths[i]
    -- max |<dir*RBA,+-hwidths>| = <|dir*R10|,|hwidths|>
    var depth_1   = +[i] absdirB[i]*hwidths[i]

    -- less than comparison
    var depth     = G.abs(c_depth) - (depth_0 + depth_1)
    if depth > OBB_EPSILON then return 0, NullContacts() end -- early exit
    var norm_len  = G.magnitude(dirA)
    if norm_len > OBB_EPSILON then
      var inv_len = num(1.0) / norm_len
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

local gong
function obb_isct( p0 : Planks, p1 : Planks ) : { G.uint32, ContactT[4] }
  var R0    = qRotMat(p0.rot)
  var R1    = qRotMat(p1.rot)
  R0        = :[i,j] R0[j,i]
  R1        = :[i,j] R1[j,i]

  -- compute relative linear and rotational offsets
  var dp    = p1.pos - p0.pos
  var dp0   = :[i] +[j]  R0[i,j] * dp[j]
  var dp1   = :[i] +[j] -R1[i,j] * dp[j]
  var R10   = :[i,j] +[k] R0[i,k] * R1[j,k] -- R1 is transposed
  var Q     = :[i,j] G.fabs(R10[i,j])

  -- declare variables
  var norm      : vec3    = {0f,0f,0f}
  var inv_norm  : G.bool  = false
  var min_depth = num( -math.huge )
  var case_code : G.int32 = -1
  var pen_ratio : num     = 0f

  var hwidths   = vec3(dims * 0.5f)

  -- test box-axis tests, 3 axes for p0 and 3 axes for p1 = 6 tests
  var BB_1in0   = :[i] hwidths[i] + (+[j] Q[i,j] * hwidths[j])
  var BB_0in1   = :[i] hwidths[i] + (+[j] Q[j,i] * hwidths[j])
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
  Q = :[i,j] Q[i,j] + OBB_EPSILON

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
    {-R[1][0], R[0][0], 0},  {Q[1][0], Q[0][0], 0},  {0, Q[2][2], Q[2][1]},
    dp0, hwidths, 6,      min_depth, norm, inv_norm, case_code, pen_ratio )
  edge_edge_obb(
    {-R[1][1], R[0][1], 0},  {Q[1][1], Q[0][1], 0},  {Q[2][2], 0, Q[2][0]},
    dp0, hwidths, 7,      min_depth, norm, inv_norm, case_code, pen_ratio )
  edge_edge_obb(
    {-R[1][2], R[0][2], 0},  {Q[1][2], Q[0][2], 0},  {Q[2][1], Q[2][0], 0},
    dp0, hwidths, 8,      min_depth, norm, inv_norm, case_code, pen_ratio )

  -- HERE.  We have an intersection as determined above
  var depth = -min_depth
  if inv_norm then norm = -norm end

  -- Edge-Edge collision
  if case_code >= 6 then
    -- get norms in different reference frames
    var normA       = norm
    var normB       = :[i] +[j] R10[j,i] * -norm[j]
    var world_norm  = :[i] +[j] R0[j,i] * norm[j]
    -- axes involved
    case_code       = case_code - 6
    var axisA       = case_code / 3
    var axisB       = case_code % 3
    var A0          = (axisA+1)%3
    var A1          = (axisA+2)%3
    var B0          = (axisB+1)%3
    var B1          = (axisB+2)%3
    -- sign of axes
    var sA0         = ( normA[A0] < 0 )? num(-1.f) else num(1.f)
    var sA1         = ( normA[A1] < 0 )? num(-1.f) else num(1.f)
    var sB0         = ( normB[B0] < 0 )? num(-1.f) else num(1.f)
    var sB1         = ( normB[B1] < 0 )? num(-1.f) else num(1.f)
    -- points on edges of A,B in local frames
    var mA          = vec3({0f,0f,0f})
    var mB          = vec3({0f,0f,0f})
    mA[A0]          = sA0 * hwidths[A0]
    mA[A1]          = sA1 * hwidths[A1]
    mB[B0]          = sB0 * hwidths[B0]
    mB[B1]          = sB1 * hwidths[B1]
    -- to world frames
    mA              = :[i] p0.pos[i] + (+[j] R0[j,i]*mA[j]) 
    mB              = :[i] p1.pos[i] + (+[j] R1[j,i]*mB[j])
    -- half-edge vectors in world frames
    var hA          = :[i] R0[axisA,i] * hwidths[axisA]
    var hB          = :[i] R1[axisB,i] * hwidths[axisB]
    var lhA2        = hwidths[axisA] * hwidths[axisA]
    var lhB2        = hwidths[axisB] * hwidths[axisB]
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
    var CA  = +[i] (mB[i]-mA[i]) * hA[i]
    var CB  = +[i] (mB[i]-mA[i]) * hB[i]
    var AB  = +[i] hA[i]*hB[i]
    var AA  = lhA2
    var BB  = lhB2
    -- This energy is minimized by a solution to the system
    --    2*CA  =  2*AA*ta - 2*AB*tb
    --   -2*CB  = -2*AB*ta + 2*BB*tb
    -- Which by Cramer's rule is given by
    var denom = AA*BB - AB*AB
    var inv_d = num(1) / denom
    var ta    = ( BB*CA - AB*CB ) * inv_d
    var tb    = ( AB*CA - AA*CB ) * inv_d
    if G.abs(denom) < OBB_EPSILON then ta = 0; tb = 0 end
    -- clamp values
    ta = G.clamp(ta, num(-1f), num(1f))
    tb = G.clamp(tb, num(-1f), num(1f))
    -- compute the final point as the average of these points
    var pt    = :[i] num(0.5f) * (mA[i] + hA[i]*ta + mB[i] + hB[i]*tb)

    return 1, {
      { pt = pt,               norm = world_norm,       depth = depth },
      { pt = vec3({0f,0f,0f}), norm = vec3({0f,0f,0f}), depth = 0f },
      { pt = vec3({0f,0f,0f}), norm = vec3({0f,0f,0f}), depth = 0f },
      { pt = vec3({0f,0f,0f}), norm = vec3({0f,0f,0f}), depth = 0f }
    }

  -- face-body collision
  else
    if case_code >= 3 then -- flip plank 0 and 1 roles
      R10         = :[i,j] R10[j,i]
      swap(R0,R1)
      swap(dp0,dp1)
      swap(p0,p1)
    end
    -- compute axis codes
    var axisA     = case_code % 3
    var invA      = (dp0[axisA] < 0f)? num(-1f) else num(1f)
    var Anorm_inB = :[i] R10[axisA,i]
    var axisB     = vMaxAbsAxis( Anorm_inB )
    -- TODO: Should be greater than or less than?
    G.assert(false)
    var invB      = invA * ((Anorm_inB[axisB] > 0)? num(-1f) else num(1f))
    var A0        = (axisA+1)%3
    var A1        = (axisA+2)%3
    var B0        = (axisB+1)%3
    var B1        = (axisB+2)%3

    -- define a 2d coordinate system F spanned by A0,A1
    -- extract face rect bounds from A in F
    var Fa01      = { hwidths[A0], hwidths[A1] }
    -- center of B's face in A's 3d coordinates
    var centerB   = :[i] dp0[i] + R10[i,axisB] * (hwidths[axisB]*invB)
    var cBF       = { centerB[A0], centerB[A1] }
    -- axes of B's face in F
    var Fb0       = { R10[A0][B0]*hwidths[B0], R10[A1][B0]*hwidths[B0] }
    var Fb1       = { R10[A0][B1]*hwidths[B1], R10[A1][B1]*hwidths[B1] }
    -- 4 vertices of the face of B in F
    var Fbvs      : vec2[4] = {
      cBF + Fb0 + Fb1,
      cBF - Fb0 + Fb1,
      cBF - Fb0 - Fb1,
      cBF + Fb0 - Fb1
    }

    -- compute the clipped points
    var n_pts, clip_pts = clipQuad(Fa01, Fbvs)

    -- UNPROJECT the clipped points back to 3d
    var A_pts     : vec3[8] = {{0f,0f},{0f,0f},{0f,0f},{0f,0f},
                               {0f,0f},{0f,0f},{0f,0f},{0f,0f}}
    var depths    : num[8]  = {0f,0f,0f,0f,0f,0f,0f,0f}
    -- depth coordinates
    var c_depth   : num   = centerB[axisA]
    var Fbdepth0  : num   = R10[axisA,B0] * hwidths[B0]
    var Fbdepth1  : num   = R10[axisA,B1] * hwidths[B1]
    var Acutoff           = hwidths[axisA]
    -- solve  Fb * (a0,a1) = 2d_pt - cBF  to unproject
    var Fbdet     : num   = Fb0[0]*Fb1[1] - Fb0[1]*Fb1[0]
    var inv_Fbdet : num   = num(1) / Fbdet
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
      R10         = :[i,j] R10[j,i]
      swap(R0,R1)
      swap(dp0,dp1)
      swap(p0,p1)
    end
    var cs : ContactT[4] = NullContacts()
    --var mass_per = mass/num(n_pts)
    for k=0,n_pts do
      cs[k].pt        = :[i] +[j] R0[j,i] * A_pts[k][j]
      cs[k].norm      = norm
      cs[k].depth     = depths[k]
    end
    return n_pts, cs
  end
end

local gong function compute_friction(norm : vec3)
  var e     = vec3({0,0,0})
  e[ vMinAbsAxis(norm) ] = 1
  var f0    = normed( G.cross(norm,e) )
  var f1    = G.cross(norm, f0)
  return f0,f1
end


------------------------------------------------------------------------------
-- Join

local gong join find_plank_iscts ( p0 : Planks, p1 : Planks )
  where p0 ~= p1
  var n_pts, contacts = obb_isct(p0, p1)
  where n_pts > 0
do
  var bases : ContactBase[4] = NullBases()
  for k=0,n_pts do
    bases[k].friction_0, bases[k].friction_1
                      = compute_friction(contacts[k].norm)
  end
  emit  { p0=p0, p1=p1,
          n_pts       = n_pts,
          pts         = {
            { contact=contacts[0], basis=bases[0], l_mult=vec3({0,0,0}) },
            { contact=contacts[1], basis=bases[1], l_mult=vec3({0,0,0}) },
            { contact=contacts[2], basis=bases[2], l_mult=vec3({0,0,0}) },
            { contact=contacts[3], basis=bases[3], l_mult=vec3({0,0,0}) }
          }
  } in PPContacts
end




