
local Exports = {}
package.loaded["prelude"] = Exports




local C       = terralib.includecstring [[
#include "stdlib.h"
#include "math.h"
#include "stdio.h"
#include "sys/time.h"
#include "unistd.h"
#include "float.h"
float _GET_FLT_EPSILON() { return FLT_EPSILON; }
double _GET_DBL_EPSILON() { return DBL_EPSILON; }
]]
Exports.C = C
Exports.FLT_EPSILON = C._GET_FLT_EPSILON()
Exports.DBL_EPSILON = C._GET_DBL_EPSILON()

Exports.taketime = macro(function()
  return quote
    var tv : C.timeval
    C.gettimeofday(&tv,nil)
  in [double](tv.tv_sec) + 1e-6*[double](tv.tv_usec) end
end)

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
--
------------------------------------------------------------------------------
-- Extend the terra primitive type functionalities

function Exports.API_Extend(API)
  local vec3  = API.vec3:terratype()
  local mat3  = API.mat3:terratype()
  local quat  = API.quat:terratype()
  local num   = API.num:terratype()

  if vec3.metamethods.__add then
    return
    -- Already Extended this API
  end

  -- useful constructor function/macros
  local v3 = macro(function(x,y,z)
    return `vec3{array(num(x),num(y),num(z))} end)
  local m3 = macro(function(a00,a01,a02, a10,a11,a12, a20,a21,a22)
    return `mat3{array(num(a00),num(a10),num(a20),
                       num(a01),num(a11),num(a21),
                       num(a02),num(a12),num(a22))} end)
  local q4 = macro(function(x,y,z,w)
    return `quat{array(num(x),num(y),num(z),num(w))} end)

  vec3.metamethods.__apply = macro(function(v,i)
    return `v.d[i]
  end)
  mat3.metamethods.__apply = macro(function(m,i,j)
    return `m.d[i+j*3] -- col major
  end)
  quat.metamethods.__apply = macro(function(q,i)
    return `q.d[i]
  end)

  vec3.metamethods.__add = macro(function(a,b)
    local atyp = a:gettype()
    assert(atyp == vec3 and atyp == b:gettype(), 'bad vec3 __add args')
    return quote var lhs = a; var rhs = b
    in v3(lhs(0)+rhs(0), lhs(1)+rhs(1), lhs(2)+rhs(2)) end
  end)
  vec3.metamethods.__sub = macro(function(a,b)
    local atyp = a:gettype()
    assert(atyp == vec3 and atyp == b:gettype(), 'bad vec3 __sub args')
    return quote var lhs = a; var rhs = b
    in v3(lhs(0)-rhs(0), lhs(1)-rhs(1), lhs(2)-rhs(2)) end
  end)
  vec3.metamethods.__unm = macro(function(a)
    return quote var x = a in v3(-x(0), -x(1), -x(2)) end
  end)
  mat3.metamethods.__add = macro(function(a,b)
    local atyp = a:gettype()
    assert(atyp == mat3 and atyp == b:gettype(), 'bad mat3 __add args')
    return quote var lhs = a; var rhs = b
    in m3(lhs(0,0)+rhs(0,0), lhs(0,1)+rhs(0,1), lhs(0,2)+rhs(0,2),
          lhs(1,0)+rhs(1,0), lhs(1,1)+rhs(1,1), lhs(1,2)+rhs(1,2),
          lhs(2,0)+rhs(2,0), lhs(2,1)+rhs(2,1), lhs(2,2)+rhs(2,2)) end
  end)
  mat3.metamethods.__sub = macro(function(a,b)
    local atyp = a:gettype()
    assert(atyp == mat3 and atyp == b:gettype(), 'bad mat3 __sub args')
    return quote var lhs = a; var rhs = b
    in m3(lhs(0,0)-rhs(0,0), lhs(0,1)-rhs(0,1), lhs(0,2)-rhs(0,2),
          lhs(1,0)-rhs(1,0), lhs(1,1)-rhs(1,1), lhs(1,2)-rhs(1,2),
          lhs(2,0)-rhs(2,0), lhs(2,1)-rhs(2,1), lhs(2,2)-rhs(2,2)) end
  end)
  mat3.metamethods.__unm = macro(function(a)
    return quote var x = a in m3(-x(0,0), -x(0,1), -x(0,2),
                                 -x(1,0), -x(1,1), -x(1,2),
                                 -x(2,0), -x(2,1), -x(2,2)) end
  end)

  mat3.metamethods.__mul = macro(function(lhs, rhs)
    if lhs:gettype():isarithmetic() then
      lhs,rhs = rhs,lhs
    end
    assert(lhs:gettype() == mat3, 'expected mat3 on left of multiplication')
    local rtyp = rhs:gettype()
    if rtyp == mat3 then
      return quote
        var m : mat3
        var x = lhs
        var y = rhs
        for j=0,3 do for i=0,3 do m(i,j) = x(i,0)*y(0,j) +
                                           x(i,1)*y(1,j) +
                                           x(i,2)*y(2,j) end end
      in m end
    elseif rtyp == vec3 then
      return quote
        var m = lhs
        var x = rhs
        var r : vec3
        for i=0,3 do r(i) = m(i,0)*x(0) + m(i,1)*x(1) + m(i,2)*x(2) end
      in r end
    elseif rtyp:isarithmetic() then
      return quote
        var m = lhs
        var a = rhs
        for k=0,9 do m.d[k] = m.d[k] * a end
      in m end
    else error('unexpected rhs type '..tostring(rtyp)..
               ' for matrix multiplication') end
  end)

  vec3.metamethods.__mul = macro(function(lhs, rhs)
    local ltyp = lhs:gettype()
    local rtyp = rhs:gettype()
    if ltyp:isarithmetic() then
      lhs,rhs,ltyp,rtyp = rhs,lhs,rtyp,ltyp
    end
    assert(ltyp == vec3, 'bad vec3 __mul args')
    if rtyp:isarithmetic() then
      return quote
        var x = lhs
        var a = num(rhs)
        for k=0,3 do x.d[k] = x.d[k] * a end
      in x end
    else error('unexpected rhs type '..tostring(rtyp)..
               ' for vector multiplication') end
  end)

  quat.metamethods.__mul = macro(function(lhs,rhs)
    assert( lhs:gettype() == rhs:gettype(), 'expected two quats to __mul' )
    return `q4(
      lhs(0)*rhs(3) + lhs(3)*rhs(0) + lhs(1)*rhs(2) - lhs(2)*rhs(1),
      lhs(1)*rhs(3) + lhs(3)*rhs(1) + lhs(2)*rhs(0) - lhs(0)*rhs(2),
      lhs(2)*rhs(3) + lhs(3)*rhs(2) + lhs(0)*rhs(1) - lhs(1)*rhs(0),
      lhs(3)*rhs(3) - lhs(0)*rhs(0) - lhs(1)*rhs(1) - lhs(2)*rhs(2)
    )
  end)

  terra vec3:dot( rhs : vec3 )
    var lhs = @self
    return lhs(0)*rhs(0) + lhs(1)*rhs(1) + lhs(2)*rhs(2)
  end
  terra vec3:cross( rhs : vec3 )
    var lhs = @self
    return v3( lhs(1)*rhs(2) - lhs(2)*rhs(1),
              -lhs(0)*rhs(2) + lhs(2)*rhs(0),
               lhs(0)*rhs(1) - lhs(1)*rhs(0) )
  end
  terra vec3:norm()
    return C.sqrt(self:dot(@self))
  end
  terra quat:normed()
    var q = @self
    var n = num(1)/num(C.sqrt( q(0)*q(0) + q(1)*q(1) + q(2)*q(2) + q(3)*q(3) ))
    return q4(q(0)*n, q(1)*n, q(2)*n, q(3)*n)
  end

  terra mat3:transpose()
    var m   = @self
    return m3( m(0,0), m(1,0), m(2,0),
               m(0,1), m(1,1), m(2,1),
               m(0,2), m(1,2), m(2,2) )
  end

  terra mat3:det()
    var m   = @self
    return m(0,0) * ( m(1,1)*m(2,2) - m(1,2)*m(2,1) )
         - m(0,1) * ( m(1,0)*m(2,2) - m(1,2)*m(2,0) )
         + m(0,2) * ( m(1,0)*m(2,1) - m(1,1)*m(2,0) )
  end

  mat3.methods.subdet = macro(function(m,i,j)
    i = i:asvalue()
    j = j:asvalue()
    assert(type(i) == 'number' and type(j) == 'number')
    local i0,i1 = (i==0 and 1) or 0, (i==2 and 1) or 2
    local j0,j1 = (j==0 and 1) or 0, (j==2 and 1) or 2
    return `( m(i0,j0)*m(i1,j1) - m(i0,j1)*m(i1,j0) )
  end)
  terra mat3:invert()
    var m   = @self
    var d   = self:det()
    var inv = num(1)/d
    return inv * m3(  m:subdet(0,0), -m:subdet(1,0),  m:subdet(2,0),
                     -m:subdet(0,1),  m:subdet(1,1), -m:subdet(2,1),
                      m:subdet(0,2), -m:subdet(1,2),  m:subdet(2,2) )
  end

  terra vec3:crossmat()
    var v0, v1, v2 = self(0), self(1), self(2)
    return m3(  0, -v2,  v1,
               v2,   0, -v0,
              -v1,  v0,   0 )
  end

  terra quat:conj() : quat
    var q   = @self
    return q4(-q(0), -q(1), -q(2), q(3))
  end
  terra quat:toRotMat() : mat3
    var q   = self
    var s   = q(3)
    var q0  = q(0)
    var q1  = q(1)
    var q2  = q(2)
    var m = m3(
      1 - 2*(q1*q1 + q2*q2),    2*(q0*q1 - q2*s),     2*(q0*q2 + q1*s),
          2*(q0*q1 + q2*s), 1 - 2*(q0*q0 + q2*q2),    2*(q1*q2 - q0*s),
          2*(q0*q2 - q1*s),     2*(q1*q2 + q0*s), 1 - 2*(q0*q0 + q1*q1)
    )
    return m
  end
  terra quat:rotvec( v : vec3 ) : vec3
    var q = @self
    var s = q(3)
    var u = v3(q(0), q(1), q(2))
    return 2 * u:dot(v) * u
         + (s*s - u:dot(u)) * v
         + 2 * s * u:cross(v)
  end

  local INTEGRATION_THRESHOLD = 0.25 * math.pi -- quarter of a circle
  terra quat:integrateByAngVel( w : vec3 )
    var q = @self
    -- method uses exponential map between angvel vectors and quaternions
    var nw = w:norm()
    if 2.0*nw > INTEGRATION_THRESHOLD then
      nw = num(0.5 * INTEGRATION_THRESHOLD)
    end

    var axis = v3(0,0,0)
    if nw < 0.001 then -- Taylor expansion near identity
      axis = w * (num(1) - num(1.0/6.0)*nw*nw)
    else
      axis = w * num( C.sin(nw) / nw )
    end
    var cosw = C.cos(nw)
    var expw = q4( axis(0), axis(1), axis(2), cosw )
    -- normalize to prevent accumulating error drifting away from unit length
    return (expw * q):normed()
  end
end

Exports.minf = macro(function(a,b)
  return quote
    var r = [a]
    var t = [b]
  in terralib.select(t < r, t, r) end
end)
Exports.maxf = macro(function(a,b)
  return quote
    var r = [a]
    var t = [b]
  in terralib.select(t > r, t, r) end
end)
Exports.clampf = macro(function(a,_lo,_hi)
  return quote
    var r   = [a]
    var lo  = [_lo]
    var hi  = [_hi]
  in terralib.select(r < lo, lo, terralib.select(r > hi, hi, r)) end
end)

