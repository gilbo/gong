
local VDB       = require 'gong.tools.vdb'


local N,M       = 16, 4
local ctrlx     = {
  --
  { -0.25, 0.0 },
  { 0.0, 1.0 },
  { 1.0, 1.0 },
  { 1.0, 0.0 },
  --
  { 2.0, 0.0 },
  { 2.25, 0.75 },
  { 2.0, 1.5 },
  { 1.0, 1.5 },
  --
  { 1.0, 0.5 },
  { 0.0, 0.5 },
  { -0.25, 1.25 },
  { 0.0, 2.0 },
  --
  { 1.0, 2.0 },
  { 1.0, 1.0 },
  { 2.0, 1.0 },
  { 2.25, 2.0 },
  --
}

local ctrlpts = {}
for x=1,N do
  ctrlpts[x] = {}
  for y=1,M do
    local unit    = (y-1)/(M-1)
    local xcoord  = ctrlx[x][1]
    local ycoord  = 0.75*ctrlx[x][2] + 0.2*math.sin(2*(y-1))
    local zcoord  = 1.5*(unit-0.5) + 0.1*x
    ctrlpts[x][y] = { xcoord, ycoord, zcoord }
  end
end


--[[
Bezier Curve Parameters
  A U^3 + B U^2u + C Uu^2 + D u^3 = 1
  u = 0; U = 1
    A = 1
  u = 1; U = 0
    D = 1
  u = 1/2; U = 1/2
    (A+B+C+D)*1/8 = 1; B+C = 6
  u = 1/3; U = 2/3
    (8*A + 4*B + 2*C + D) = 27; 4B + 2C = 18; 2B + C = 9
  system
    2B+C = 9
     B+C = 6
     B   = 3
     C   = 3
(the problem is that as you shift the control points,
 continuity is disrupted.  Hence, not a spline.)
(the spline needs to maintain continuity at the knots...)

  B0(u)     = (1-u)^3 / 6
  B1(u)     = (3u^3 - 6u^2 + 4) / 6
  B2(u)     = (-3u^3 + 3u^2 + 3u + 1) / 6
  B3(u)     = u^3 / 6
--]]

local function bpatch_interp(x0,x1,x2,x3, y0,y1,y2,y3, u,v)
  local x = {x0,x1,x2,x3}
  local y = {y0,y1,y2,y3}
  local U = 1-u
  local V = 1-v
  local uu = { U*U*U/6.0,
               (3*u*u*u - 6*u*u + 4)/6.0,
               (-3*u*u*u + 3*u*u + 3*u + 1)/6.0,
               u*u*u/6.0 }
  local vv = { V*V*V/6.0,
               (3*v*v*v - 6*v*v + 4)/6.0,
               (-3*v*v*v + 3*v*v + 3*v + 1)/6.0,
               v*v*v/6.0 }

  -- assemble control point patch
  local P  = {}
  for i=0,3 do P[i+1] = {}; for j=0,3 do
    P[i+1][j+1] = ctrlpts[ x[i+1] ][ y[j+1] ]
  end end

  local pt = {0,0,0} -- result value
  for i=1,4 do for j=1,4 do
    local C   = P[i][j]
    local uv  = uu[i]*vv[j]
    for k=1,3 do
      pt[k] = pt[k] + uv*C[k]
    end
  end end

  return pt
end

local function clamp(x, lo, hi) return math.min(hi, math.max(lo, x)) end




local verts = {}
local tris  = {}

VDB.vbegin()
local NX        = (N+3)*4
local MY        = (M+3)*2
local xout,yout = 0,0
for i=-2,N do
for j=-2,M do
  local x0,x1,x2,x3 = clamp(i  ,1,N), clamp(i+1,1,N),
                      clamp(i+2,1,N), clamp(i+3,1,N)
  local y0,y1,y2,y3 = clamp(j  ,1,M), clamp(j+1,1,M),
                      clamp(j+2,1,M), clamp(j+3,1,M)

  for u=0,1-0.001,1.0/4.0 do
  for v=0,0.5,0.5 do
    local pt = bpatch_interp(x0,x1,x2,x3, y0,y1,y2,y3, u,v)
    VDB.point( pt[1], pt[2], pt[3] )

    local xout = 4*(i+2) + u*4
    local yout = 2*(j+2) + v*2
    verts[xout + NX*yout] = pt
    if xout > 0 and yout > 0 then
      local quadid = (xout-1) + (NX-1)*(yout-1)
      local p00, p01, p10, p11 = (xout-1) + NX*(yout-1), (xout-1) + NX*yout,
                                 xout + NX*(yout-1),     xout + NX*yout
      tris[2*quadid + 0] = { p00,p01,p10 }
      tris[2*quadid + 1] = { p01,p11,p10 }
    end
  end
  end
end
end


-- Write OFF file
do
  local OFF = io.open('clothfold.off','w')
  OFF:write('OFF\n')
  OFF:write(tostring(#verts+1)..' '..tostring(#tris+1)..' 0\n')
  -- write vertices out
  for i=0,#verts do
    local x,y,z = verts[i][1], verts[i][2], verts[i][3]
    OFF:write(x..' '..y..' '..z..'\n')
  end
  -- write triangles out
  for i=0,#tris do
    local v0,v1,v2 = tris[i][1], tris[i][2], tris[i][3]
    OFF:write('3 '..v0..' '..v1..' '..v2..'\n')
  end
  OFF:close()
end
VDB.vend()




















