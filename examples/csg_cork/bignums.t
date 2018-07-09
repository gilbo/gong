import 'gong'
local G = gong.stdlib

local Exports   = {}
package.loaded["bignums"] = Exports

local Ext       = require 'ext'

------------------------------------------------------------------------------
-- LimbInt is the basic multi-word signed integer type
--    defined by the word-count parameter


--[[
LimbInt<N>
  init    ( int32 ) : LimbInt<N>
  promote ( arg : LimbInt<Nin> ) : LimbInt<Nout>
  neg     ( arg : LimbInt<Nin> ) : LimbInt<Nout>
  add     ( lhs : LimbInt<Nlhs>, rhs : LimbInt<Nrhs> ) : LimbInt<Nout>
  sub     ( lhs : LimbInt<Nlhs>, rhs : LimbInt<Nrhs> ) : LimbInt<Nout>
  mul     ( lhs : LimbInt<Nlhs>, rhs : LimbInt<Nrhs> ) : LimbInt<Nout>
  sign    ( arg : LimbInt<Nin> ) : int32 (-1, 0, or 1)
--]]

local Limb_T        = G.uint64
local LIMB_BITS     = 64
local SIGN_BIT_OFF  = LIMB_BITS - 1
local SIGN_BIT_MASK = (terra(x:uint64) return x << SIGN_BIT_OFF end)(0x1ULL)
local SIGN_IS_SET   = G.Macro(function(xs,n)
  return gong `( G.int64(xs[n-1]) < 0 )
end)
local SIGN_INT      = G.Macro(function(xs,n)
  return gong `SIGN_IS_SET(xs,n)? -1 else 1
end)
local ONES_PATTERN  = G.Constant(Limb_T, 0x0ULL - 1)
local ZERO_PATTERN  = G.Constant(Limb_T, 0x0ULL)
local SIGN_LIMB     = G.Macro(function(xs,n)
  return gong `( G.rshift(xs[n-1], SIGN_BIT_OFF) ~= 0 )? ONES_PATTERN
                                                    else ZERO_PATTERN
end)
local function BITS_TO_LIMBS(nb) return math.ceil(nb/LIMB_BITS) end

local ADD_CARRY     = G.Macro(function(a,b,r)
  return gong `(r < a or r < b)? Limb_T(1) else Limb_T(0)
end)

local function uniq_arg1(x, body)
  local macro_body = G.Macro(body)
  return gong quote
    var a = x
  in macro_body(a) end
end
local function uniq_arg2(x, y, body)
  local macro_body = G.Macro(body)
  return gong quote
    var a = x
    var b = y
  in macro_body(a, b) end
end
  

local LimbIntCache = {}
local function LimbInt(N)
  if LimbIntCache[N] then return LimbIntCache[N] end
  local LInt      = {}
  LimbIntCache[N] = LInt

  local function limb_rep_N(typ)
    if typ:is_record() and typ.fields[1].name == 'limbs' then
      local subtyp = typ.fields[1].type
      if subtyp:is_tensor() and subtyp:basetype() == Limb_T
                            and #subtyp.dims == 1
      then
        local NN = subtyp.dims[1]
        if LimbInt(NN).rep == typ then
          return NN
        end
      end
    end
    return false
  end

  local Rep       = G.record { {'limbs', Limb_T[N]} }
  LInt.rep        = Rep

  local RepZEROS  = {}
  for k=1,N do RepZEROS[k] = ZERO_PATTERN end

  LInt.init       = G.Macro(function(x)
    assert(x:gettype() == G.int32, 'expect int32 arg')
    if N == 1 then
      return gong `{ limbs = { Limb_T( G.int64(x) ) } }
    else
      return uniq_arg1(x, function(x)
        local initzero  = { gong `Limb_T( G.int64(x) ) }
        local initones  = { gong `Limb_T( G.int64(x) ) }
        for k=2,N do
          initzero[k]   = ZERO_PATTERN
          initones[k]   = ONES_PATTERN
        end
        return gong `{ limbs = ( x < 0 )? initones else initzero }
      end)
    end
  end)

  LInt.promote    = G.Macro(function(x)
    local Nx      = limb_rep_N(x:gettype())
    assert(Nx, 'expected LimbInt argument')
    if Nx == N then
      return x
    else
      assert(Nx < N)
      return uniq_arg1(x, function(x)
        local limbszero = {}
        local limbsones = {}
        for k=1,Nx do
          local lmb = gong `x.limbs[k-1]
          limbszero[k] = lmb
          limbsones[k] = lmb
        end
        for k=Nx+1,N do
          limbszero[k] = ZERO_PATTERN
          limbsones[k] = ONES_PATTERN
        end
        return gong `{ limbs = SIGN_IS_SET(x.limbs,Nx)? limbsones
                                                  else limbszero }
      end)
    end
  end)

  -- Careful that if you have a 100000... pattern,
  -- then you will just get that pattern back
  LInt.neg        = G.Macro(function(x)
    local Nx      = limb_rep_N(x:gettype())
    assert(Nx, 'expected LimbInt argument')
    assert(N >= Nx)
    return gong quote
      var a       = LInt.promote(x)
      var carry   = Limb_T(1)
      for k=0,N do
        var mid   = G.bitnot(a.limbs[k]) + carry
        if mid ~= 0 then carry = 0 end
        a.limbs[k] = mid
      end
    in a end
  end)

  LInt.sign       = G.Macro(function(x)
    local Nx      = limb_rep_N(x:gettype())
    assert(Nx, 'expected LimbInt argument')
    assert(Nx == N, 'expected declared input size for sign()')
    return gong quote
      var a       = x
      var nonzero = false
      for k=0,N do
        if a.limbs[k] ~= 0 then nonzero = true; break end
      end
    in SIGN_INT(a.limbs, N) * (nonzero? 1 else 0) end
  end)

  LInt.add        = G.Macro(function(x,y)
    local Nx      = limb_rep_N(x:gettype())
    assert(Nx, 'expected LimbInt argument')
    local Ny      = limb_rep_N(y:gettype())
    assert(Ny, 'expected LimbInt argument')
    assert(N >= Nx and N >= Ny)

    return gong quote
      var A     = LInt.promote(x)
      var B     = LInt.promote(y)
      var carry = Limb_T(0)
      for k=0,Ny do
        var a,b = A.limbs[k], B.limbs[k]
        var r   = a + b + carry
        carry   = ADD_CARRY(a,b,r)
        A.limbs[k] = r
      end
    in A end
  end)

  LInt.sub        = G.Macro(function(x,y)
    local Nx      = limb_rep_N(x:gettype())
    assert(Nx, 'expected LimbInt argument')
    local Ny      = limb_rep_N(y:gettype())
    assert(Ny, 'expected LimbInt argument')
    assert(N >= Nx and N >= Ny)

    return gong quote
      var xx    = x
      var yy    = y
      var A     = LInt.promote(xx)
      var B     = LInt.neg(yy)
      --G.print('sub', A.limbs)
      --G.print('   ', B.limbs)
      --G.print('  y', yy.limbs)
      --G.print('  x', xx.limbs)
    in LInt.add(A,B) end
  end)

  LInt.mul        = G.Macro(function(x,y)
    local Nx      = limb_rep_N(x:gettype())
    assert(Nx, 'expected LimbInt argument')
    local Ny      = limb_rep_N(y:gettype())
    assert(Ny, 'expected LimbInt argument')
    assert(N >= Nx and N >= Ny)
    local NA, NB = Nx, Ny

    local AInt    = LimbInt(Nx)
    local BInt    = LimbInt(Ny)

    return gong quote
      var R     : Rep = { limbs = RepZEROS }
      var A, B  = x, y
      var sA,sB = SIGN_IS_SET(A.limbs,NA), SIGN_IS_SET(B.limbs,NB)
      if sA then A = AInt.neg(A) end
      if sB then B = BInt.neg(B) end

      -- now do multiply assuming positive numbers
      for ia = 0,NA do
        var a = A.limbs[ia]
        var carry = Limb_T(0)
        for ib = 0,NB do
          if ia+ib >= N then break end -- don't write past the end

          --G.print('    --  STATUS ',ia,ib,' - ',
          --        A.limbs,B.limbs,'=',R.limbs)
          var b       = B.limbs[ib]
          var r       = R.limbs[ia+ib]
          var lo,hi   = G.mul_lohi(a,b)
          -- worst case is (2^64 - 1)*(2^64 - 1) + (2^64 - 1) + (2^64 - 1)
          --       being   A[ia]*B[ib] + R[ia+ib] + carry
          -- This value (the largest possible value) is
          --    = (2^64)*(2^64-1) + (2^64-1)
          --    = 2^128 - 2^64 + 2^64 - 1    = 2^128 - 1
          -- Which means the result will ALWAYS fit into two 64-bit words
          -- * however, we must work out the carry behavior for
          --   R[ia+ib] and carry both being accumulated into "lo"
          var r_carry = r + carry
          carry       = ADD_CARRY(r, carry, r_carry)
          var r_lo_c  = r_carry + lo
          carry       = carry + ADD_CARRY(r_carry, lo, r_lo_c)
          carry       = carry + hi
          R.limbs[ia+ib] = r_lo_c
        end
        var ib = NB -- R[ia+ib] == 0 by construction
        if ia+ib < N then
          R.limbs[ia+ib] = carry -- deposit any extra high order bits
        end
      end

      -- final sign correction if needed
      if sA ~= sB then R = LInt.neg(R) end
      --G.print('MUL ',sA,sB,A.limbs,B.limbs,'=',R.limbs)
    in R end
  end)

  local MAX64 = math.pow(2,64)
  LInt.to_double  = G.Macro(function(x)
    local Nx      = limb_rep_N(x:gettype())
    assert(Nx, 'expected LimbInt argument')
    assert(N == Nx)
    return gong quote
      var val : G.double = 0.0
      var a   = x
      var sa  = SIGN_IS_SET(a.limbs,N)
      if sa then a = LInt.neg(a) end
      for k=0,N do
        var i = N-k-1
        val = MAX64 * val + G.double(a.limbs[i])
      end
      if sa then val = -val end
    in val end
  end)

  return LInt
end

------------------------------------------------------------------------------
-- BitInt is an interface for accessing LimbInt that
--    converts from a bit count to the word count automatically

local function BitInt(nbits)
  local nlimbs = BITS_TO_LIMBS(nbits)
  return LimbInt(nlimbs)
end
Exports.BitInt = BitInt

------------------------------------------------------------------------------
-- FixExt is an exterior calculus system based on these exact numbers

local FE41_cache = {}
local FE42_cache = {}
local FE43_cache = {}

local function FixExt_4_1(nb) -- number of bits
  if FE41_cache[nb] then return FE41_cache[nb] end

  local LInt      = BitInt(nb).rep
  local Rep       = G.record { { 'e0', LInt },
                               { 'e1', LInt },
                               { 'e2', LInt },
                               { 'e3', LInt } }

  FE41_cache[nb]  = Rep
  return Rep
end
local function FixExt_4_2(nb) -- number of bits
  if FE42_cache[nb] then return FE42_cache[nb] end

  local LInt      = BitInt(nb).rep
  local Rep       = G.record { { 'e01', LInt },
                               { 'e02', LInt },
                               { 'e03', LInt },
                               { 'e12', LInt },
                               { 'e13', LInt },
                               { 'e23', LInt } }

  FE42_cache[nb]  = Rep
  return Rep
end
local function FixExt_4_3(nb) -- number of bits
  if FE43_cache[nb] then return FE43_cache[nb] end

  local LInt      = BitInt(nb).rep
  local Rep       = G.record { { 'e012', LInt },
                               { 'e013', LInt },
                               { 'e023', LInt },
                               { 'e123', LInt } }

  FE43_cache[nb]  = Rep
  return Rep
end

Exports.FixExt_4_1    = FixExt_4_1
Exports.FixExt_4_2    = FixExt_4_2
Exports.FixExt_4_3    = FixExt_4_3

-------------------------------

local function FExt(dim, bits)
  if      dim == 1 then   return FixExt_4_1(bits)
  elseif  dim == 2 then   return FixExt_4_2(bits)
  elseif  dim == 3 then   return FixExt_4_3(bits)
  else assert('UNEXPECTED CALL with dim: '..dim) end
end
local function getFEdim( typ )
  assert( typ:is_record() )
  local nm1 = typ.fields[1].name
  if      nm1 == 'e0'   then    return 1
  elseif  nm1 == 'e01'  then    return 2
  elseif  nm1 == 'e012' then    return 3
  else assert('UNEXPECTED CALL with type: '..tostring(typ)) end
end

local function setmethodname(f, nm, dim1, bits1, dim2, bits2)
  nm = nm..'_FExt_'..dim1..'b'..bits1
  if dim2 then
    nm = nm..'_'..dim2..'b'..bits2
  end
  f:setname(nm)
  return f
end

-------------------------------

local neg_cache = { {}, {}, {} } -- subcaches 1, 2, 3
local function get_neg( xDim, xBits, xRep )
  if neg_cache[xDim][xBits] then return neg_cache[xDim][xBits] end

  local xLInt = BitInt(xBits)
  local negf  = nil
  if      xDim == 1 then
    negf = gong function( x : xRep ) : xRep
      var r : xRep = {
        e0 = xLInt.neg(x.e0),
        e1 = xLInt.neg(x.e1),
        e2 = xLInt.neg(x.e2),
        e3 = xLInt.neg(x.e3)
      }
      return r
    end
  elseif  xDim == 2 then
    negf = gong function( x : xRep ) : xRep
      var r : xRep = {
        e01 = xLInt.neg(x.e01),
        e02 = xLInt.neg(x.e02),
        e03 = xLInt.neg(x.e03),
        e12 = xLInt.neg(x.e12),
        e13 = xLInt.neg(x.e13),
        e23 = xLInt.neg(x.e23)
      }
      return r
    end
  elseif  xDim == 3 then
    negf = gong function( x : xRep ) : xRep
      var r : xRep = {
        e012 = xLInt.neg(x.e012),
        e013 = xLInt.neg(x.e013),
        e023 = xLInt.neg(x.e023),
        e123 = xLInt.neg(x.e123)
      }
      return r
    end
  end
  setmethodname(negf, 'neg', xDim, xBits)

  neg_cache[xDim][xBits] = negf
  return negf
end
local neg = G.Macro( function( xBits, x )
  local xRep  = x:gettype()
  local xDim  = getFEdim( xRep )
  local negf  = get_neg(xDim, xBits:get_value(), xRep)
  return gong `negf(x)
end)
Exports.neg   = neg


local dual_cache    = { {}, {}, {} } -- subcaches 1, 2, 3
local revdual_cache = { {}, {}, {} } -- subcaches 1, 2, 3
local function get_dual( xDim, xBits, xRep )
  if dual_cache[xDim][xBits] then return dual_cache[xDim][xBits] end

  local xLInt     = BitInt(xBits)
  local dualf     = nil
  if      xDim == 1 then
    local rRep = FExt(3, xBits)
    dualf = gong function( x : xRep ) : rRep
      var r : rRep = {
        e012 = x.e3,
        e013 = xLInt.neg(x.e2),
        e023 = x.e1,
        e123 = xLInt.neg(x.e0)
      }
      return r
    end
  elseif  xDim == 2 then
    local rRep = FExt(2, xBits)
    dualf = gong function( x : xRep ) : rRep
      var r : rRep = {
        e01 = x.e23,
        e02 = xLInt.neg(x.e13),
        e03 = x.e12,
        e12 = x.e03,
        e13 = xLInt.neg(x.e02),
        e23 = x.e01
      }
      return r
    end
  elseif  xDim == 3 then
    local rRep = FExt(1, xBits)
    dualf = gong function( x : xRep ) : rRep
      var r : rRep = {
        e0 = x.e123,
        e1 = xLInt.neg(x.e023),
        e2 = x.e013,
        e3 = xLInt.neg(x.e012)
      }
      return r
    end
  end
  setmethodname(dualf, 'dual', xDim, xBits)

  dual_cache[xDim][xBits] = dualf
  return dualf
end
local function get_revdual( xDim, xBits, xRep )
  if revdual_cache[xDim][xBits] then return revdual_cache[xDim][xBits] end

  local xLInt     = BitInt(xBits)
  local revdualf  = nil
  if      xDim == 1 then
    local rRep = FExt(3, xBits)
    revdualf = gong function( x : xRep ) : rRep
      var r : rRep = {
        e012 = xLInt.neg(x.e3),
        e013 = x.e2,
        e023 = xLInt.neg(x.e1),
        e123 = x.e0
      }
      return r
    end
  elseif  xDim == 2 then
    local rRep = FExt(2, xBits)
    revdualf = gong function( x : xRep ) : rRep
      var r : rRep = {
        e01 = x.e23,
        e02 = xLInt.neg(x.e13),
        e03 = x.e12,
        e12 = x.e03,
        e13 = xLInt.neg(x.e02),
        e23 = x.e01
      }
      return r
    end
  elseif  xDim == 3 then
    local rRep = FExt(1, xBits)
    revdualf = gong function( x : xRep ) : rRep
      var r : rRep = {
        e0 = xLInt.neg(x.e123),
        e1 = x.e023,
        e2 = xLInt.neg(x.e013),
        e3 = x.e012
      }
      return r
    end
  end
  setmethodname(revdualf, 'revdual', xDim, xBits)

  revdual_cache[xDim][xBits] = revdualf
  return revdualf
end
local dual = G.Macro( function( xBits, x )
  local xRep      = x:gettype()
  local xDim      = getFEdim( xRep )
  local dualf     = get_dual(xDim, xBits:get_value(), xRep)
  return gong `dualf(x)
end)
local revdual = G.Macro( function( xBits, x )
  local xRep      = x:gettype()
  local xDim      = getFEdim( xRep )
  local revdualf  = get_revdual(xDim, xBits:get_value(), xRep)
  return gong `revdualf(x)
end)
Exports.dual      = dual
Exports.revdual   = revdual

-------------------------------

local inner_cache = { {}, {}, {} }
local function get_inner( dim, xBits, xRep, yBits, yRep )
  local subc = inner_cache[dim][xBits]
  if not subc then subc = {}; inner_cache[dim][xBits] = subc end
  if subc[yBits] then return subc[yBits] end

  local xInt      = BitInt(xBits)
  local yInt      = BitInt(yBits)
  local pInt      = BitInt(xBits + yBits)
  local abInt     = BitInt(xBits + yBits + 1)
  local rInt      = BitInt(xBits + yBits + 2)
  local rrInt     = BitInt(xBits + yBits + 3)
  local innerf    = nil
  if      dim == 1 then
    innerf = gong function( x : xRep, y : yRep ) : rInt.rep
      var p0  = pInt.mul( x.e0, y.e0 )
      var p1  = pInt.mul( x.e1, y.e1 )
      var p2  = pInt.mul( x.e2, y.e2 )
      var p3  = pInt.mul( x.e3, y.e3 )
      var a   = abInt.add( p0, p1 )
      var b   = abInt.add( p2, p3 )
      var r   = rInt.add( a, b )
      return r
    end
  elseif  dim == 2 then
    innerf = gong function( x : xRep, y : yRep ) : rrInt.rep
      var p0  = pInt.mul( x.e01, y.e01 )
      var p1  = pInt.mul( x.e02, y.e02 )
      var p2  = pInt.mul( x.e03, y.e03 )
      var p3  = pInt.mul( x.e12, y.e12 )
      var p4  = pInt.mul( x.e13, y.e13 )
      var p5  = pInt.mul( x.e23, y.e23 )
      var a   = abInt.add( p0, p1 )
      var b   = abInt.add( p2, p3 )
      var c   = abInt.add( p4, p5 )
      var x   = rInt.add( a, b )
      var rr  = rrInt.add( x, c )
      return rr
    end
  elseif  dim == 3 then
    innerf = gong function( x : xRep, y : yRep ) : rInt.rep
      var p0  = pInt.mul( x.e012, y.e012 )
      var p1  = pInt.mul( x.e013, y.e013 )
      var p2  = pInt.mul( x.e023, y.e023 )
      var p3  = pInt.mul( x.e123, y.e123 )
      var a   = abInt.add( p0, p1 )
      var b   = abInt.add( p2, p3 )
      var r   = rInt.add( a, b )
      return r
    end
  end
  setmethodname(innerf, 'inner', dim, xBits, dim, yBits)

  subc[yBits] = innerf
  return innerf
end
local inner = G.Macro( function( xBits, x, yBits, y )
  local xRep      = x:gettype()
  local yRep      = y:gettype()
  local dim       = getFEdim( xRep )
  assert( dim == getFEdim( yRep ), 'must have matching dimensions')
  local innerf    = get_inner(dim, xBits:get_value(), xRep,
                                   yBits:get_value(), yRep)
  return gong `innerf(x,y)
end)
Exports.inner = inner

-------------------------------

local sign = G.Macro( function( x )
  local xtyp      = x:gettype()
  assert(xtyp:is_record() and
         #xtyp.fields == 1 and
         xtyp.fields[1].name == 'limbs')
  xtyp            = xtyp.fields[1].type
  assert(xtyp:is_tensor())
  assert(#xtyp.dims == 1)
  local N         = xtyp.dims[1]
  assert(xtyp:basetype() == Limb_T)
  local LInt      = LimbInt(N)
  return gong `LInt.sign(x)
end)
Exports.sign = sign

-------------------------------

local join_cache = {}
local function get_join( xDim, xBits, xRep, yDim, yBits, yRep )
  local key = tostring(xDim)..'_'..tostring(yDim)..'_'..
              tostring(xBits)..'_'..tostring(yBits)
  if join_cache[key] then return join_cache[key] end

  local xInt      = BitInt(xBits)
  local yInt      = BitInt(yBits)
  local abInt     = BitInt(xBits+yBits)
  local rInt      = BitInt(xBits+yBits+1)
  local rrInt     = BitInt(xBits+yBits+2)
  local mul       = abInt.mul
  local joinf    = nil
  if      xDim == 1 and yDim == 1 then
    local rRep    = FExt(2, xBits + yBits + 1)
    joinf = gong function( x : xRep, y : yRep ) : rRep
      --G.print('*\n**\n***\n****')
      var r : rRep = {
        e01   = rInt.sub( mul( x.e0, y.e1 ), mul( y.e0, x.e1 ) ),
        e02   = rInt.sub( mul( x.e0, y.e2 ), mul( y.e0, x.e2 ) ),
        e03   = rInt.sub( mul( x.e0, y.e3 ), mul( y.e0, x.e3 ) ),
        e12   = rInt.sub( mul( x.e1, y.e2 ), mul( y.e1, x.e2 ) ),
        e13   = rInt.sub( mul( x.e1, y.e3 ), mul( y.e1, x.e3 ) ),
        e23   = rInt.sub( mul( x.e2, y.e3 ), mul( y.e2, x.e3 ) )
      }
      --G.print('****\n***\n**\n*')
      return r
    end
  elseif  xDim == 2 and yDim == 1 then
    local rrRep   = FExt(3, xBits + yBits + 2)
    joinf = gong function( x : xRep, y : yRep ) : rrRep
      --G.print('*\n**\n***\n****\n*****')
      --G.print('x:',x.e01.limbs, x.e02.limbs, x.e03.limbs)
      --G.print('  ',x.e12.limbs, x.e13.limbs, x.e23.limbs)
      --G.print('y:',y.e0.limbs, y.e1.limbs, y.e2.limbs, y.e3.limbs)
      --G.print('y:',y.e0.limbs, y.e1.limbs, y.e2.limbs, y.e3.limbs)
      var r : rrRep = {
        e012  = rrInt.add( rInt.sub( mul(x.e01, y.e2), mul(x.e02, y.e1) ),
                           mul(x.e12, y.e0) ),
        e013  = rrInt.add( rInt.sub( mul(x.e01, y.e3), mul(x.e03, y.e1) ),
                           mul(x.e13, y.e0) ),
        e023  = rrInt.add( rInt.sub( mul(x.e02, y.e3), mul(x.e03, y.e2) ),
                           mul(x.e23, y.e0) ),
        e123  = rrInt.add( rInt.sub( mul(x.e12, y.e3), mul(x.e13, y.e2) ),
                           mul(x.e23, y.e1) )
      }
      --G.print('*****\n****\n***\n**\n*')
      return r
    end
  else
    error('bad dimensions: '..xDim..' '..yDim)
  end
  setmethodname(joinf, 'join', xDim, xBits, yDim, yBits)

  join_cache[key] = joinf
  return joinf
end
local join = G.Macro( function( xBits, x, yBits, y )
  local xRep      = x:gettype()
  local yRep      = y:gettype()
  local xDim      = getFEdim( xRep )
  local yDim      = getFEdim( yRep )
  if xDim < yDim then
    xRep, yRep    = yRep, xRep
    xDim, yDim    = yDim, xDim
    xBits, yBits  = yBits, xBits
    x,y           = y,x
  end
  local joinf     = get_join(xDim, xBits:get_value(), xRep,
                             yDim, yBits:get_value(), yRep)
  return gong `joinf(x,y)
end)
Exports.join = join

-------------------------------

local meet_cache = {}
local function get_meet( xDim, xBits, xRep, yDim, yBits, yRep )
  local key = tostring(xDim)..'_'..tostring(yDim)..'_'..
              tostring(xBits)..'_'..tostring(yBits)
  if meet_cache[key] then return meet_cache[key] end

  local meetf    = nil
  if      xDim == 3 and yDim == 3 then
    local jBits   = xBits + yBits + 1
    local rRep    = FExt(2, jBits)
    meetf = gong function( x : xRep, y : yRep ) : rRep
      return revdual( jBits, join( xBits, dual(xBits, x),
                                   yBits, dual(yBits, y) ) )
    end
  elseif  xDim == 2 and yDim == 3 then
    local jBits   = xBits + yBits + 2
    local rrRep   = FExt(1, jBits)
    meetf = gong function( x : xRep, y : yRep ) : rrRep
      return revdual( jBits, join( xBits, dual(xBits, x),
                                   yBits, dual(yBits, y) ) )
    end
  else
    error('bad dimensions: '..xDim..' '..yDim)
  end
  setmethodname(meetf, 'meet', xDim, xBits, yDim, yBits)

  meet_cache[key] = meetf
  return meetf
end
local meet = G.Macro( function( xBits, x, yBits, y )
  local xRep      = x:gettype()
  local yRep      = y:gettype()
  local xDim      = getFEdim( xRep )
  local yDim      = getFEdim( yRep )
  if xDim > yDim then
    xRep, yRep    = yRep, xRep
    xDim, yDim    = yDim, xDim
    xBits, yBits  = yBits, xBits
    x,y           = y,x
  end
  local meetf     = get_meet(xDim, xBits:get_value(), xRep,
                             yDim, yBits:get_value(), yRep)
  return gong `meetf(x,y)
end)
Exports.meet = meet

-------------------------------

local to_ext_cache = { {}, {}, {} }
to_ext_cache[0] = {}
local function get_to_ext( xDim, nlimbs, xRep )
  if to_ext_cache[xDim][nlimbs] then return to_ext_cache[xDim][nlimbs] end

  local xLInt     = LimbInt(nlimbs)
  local to_extf   = nil
  if      xDim == 0 then
    to_extf = gong function( x : xRep, a : G.double ) : G.double
      return xLInt.to_double(x) * a
    end
  elseif  xDim == 1 then
    local Ext4_1  = Ext.Ext4_1
    to_extf = gong function( x : xRep, a : G.double ) : Ext4_1
      var r : Ext4_1 = {
        e0 = xLInt.to_double(x.e0) * a,
        e1 = xLInt.to_double(x.e1) * a,
        e2 = xLInt.to_double(x.e2) * a,
        e3 = xLInt.to_double(x.e3) * a
      }
      return r
    end
  elseif  xDim == 2 then
    local Ext4_2  = Ext.Ext4_2
    to_extf = gong function( x : xRep, a : G.double ) : Ext4_2
      var r : Ext4_2 = {
        e01 = xLInt.to_double(x.e01) * a,
        e02 = xLInt.to_double(x.e02) * a,
        e03 = xLInt.to_double(x.e03) * a,
        e12 = xLInt.to_double(x.e12) * a,
        e13 = xLInt.to_double(x.e13) * a,
        e23 = xLInt.to_double(x.e23) * a
      }
      return r
    end
  elseif  xDim == 3 then
    local Ext4_3  = Ext.Ext4_3
    to_extf = gong function( x : xRep, a : G.double ) : Ext4_3
      var r : Ext4_3 = {
        e012 = xLInt.to_double(x.e012) * a,
        e013 = xLInt.to_double(x.e013) * a,
        e023 = xLInt.to_double(x.e023) * a,
        e123 = xLInt.to_double(x.e123) * a
      }
      return r
    end
  end
  setmethodname(to_extf, 'to_ext', xDim, nlimbs)

  to_ext_cache[xDim][nlimbs] = to_extf
  return to_extf
end
local to_ext = G.Macro( function( x, scale )
  local xRep    = x:gettype()
  assert(xRep:is_record())
  local to_extf = nil
  if xRep.fields[1].name == 'limbs' then
    local xtyp    = xRep.fields[1].type
    assert(xtyp:is_tensor() and #xtyp.dims == 1)
    local N       = xtyp.dims[1]
    to_extf       = get_to_ext(0, N, xRep)
  else
    local xtyp    = xRep.fields[1].type
    assert(xtyp:is_record() and
           #xtyp.fields == 1 and
           xtyp.fields[1].name == 'limbs')
    xtyp          = xtyp.fields[1].type
    assert(xtyp:is_tensor())
    assert(#xtyp.dims == 1)
    local N       = xtyp.dims[1]
    local xDim    = getFEdim( xRep )
    to_extf       = get_to_ext(xDim, N, xRep)
  end
  if scale then
    return gong `to_extf(x,scale)
  else
    return gong `to_extf(x,1.0)
  end
end)
Exports.to_ext   = to_ext









