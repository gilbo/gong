import 'gong'
local G = gong.stdlib


local prelude       = require 'prelude'
local num           = prelude.num
local FLT_EPSILON   = prelude.FLT_EPSILON
local DBL_EPSILON   = prelude.DBL_EPSILON

local FExt          = require 'bignums'
local BitInt        = FExt.BitInt
local FixExt_4_1    = FExt.FixExt_4_1

local extmodule     = require 'ext'
local Ext           = extmodule.Ext
local AExt          = extmodule.AExt
local Ext4_1        = extmodule.Ext4_1

------------------------------------------------------------------------------
-- Constants / Parameters

local QUANT_BITS  = 30
local IN_BITS     = QUANT_BITS + 1;

local MAGNIFY     = G.Global('MAGNIFY', G.double, 1.0)
local RESHRINK    = G.Global('RESHRINK', G.double, 1.0)

local EPS         = DBL_EPSILON
local EPS2        = EPS * EPS


local BUFFER_EFF  = false
local BUFFER_IDX  = false
local VERIFY_IDX  = false

------------------------------------------------------------------------------
-- Support Functions / Defs

local vec3      = G.vector(num,3)

local swap = G.Macro(function(a, b)
  return gong quote a,b = b,a end
end)

local normed = G.Macro(function(a)
  return gong quote
    var b   = a
    var rn  = num(1f) / G.magnitude(b)
  in rn * b end
end)

------------------------------------------------------------------------------
-- Schema

local Vertices     = G.NewTable('Vertices')
                      :NewField('pos',  vec3)
local Edges        = G.NewTable('Edges')
                      :NewField('hd',   Vertices)
                      :NewField('tl',   Vertices)
                      :NewField('id',   G.uint64)
local Triangles    = G.NewTable('Triangles')
                      :NewField('v',    G.vector(Vertices, 3))
                      :NewField('id',   G.uint64)

local IsctResult   = G.record {
                        {'code',    G.uint8}
                      }
local gong function result_is_no_isct( ir : IsctResult )
  return ir.code == 0
end
local gong function result_is_isct( ir : IsctResult )
  return ir.code == 1
end
local gong function result_is_degen( ir : IsctResult )
  return ir.code == 2
end
local gong function NoIsctResult() : IsctResult
  return { code = G.uint8(0) }
end
local gong function YesIsctResult() : IsctResult
  return { code = G.uint8(1) }
end
local gong function DegenerateResult() : IsctResult
  return { code = G.uint8(2) }
end

local ET_Iscts     = G.NewTable('ET_Iscts')
                      :NewField('edge', Edges)
                      :NewField('tri',  Triangles)

local degeneracy_count  = G.Global('degeneracy_count',  G.uint32, 0)
local has_degeneracies  = G.Global('has_degeneracies',  G.bool, false)
local has_intersections = G.Global('has_intersections', G.bool, false)



------------------------------------------------------------------------------
-- SubRoutines


local TriEdgeInput = G.record {
                        { 'e',  vec3[2] },
                        { 't',  vec3[3] },
                      }

local gong function quantize2int( x : num )
  return G.int32( x * MAGNIFY )
end
local gong function quantize( x : num )
  return RESHRINK * num( G.int32( x * MAGNIFY ) )
end

local FixExt_4_1_IN = FixExt_4_1(IN_BITS)
local BitInt_IN     = BitInt(IN_BITS)
local gong function toFixExt( v : vec3 ) : FixExt_4_1_IN
  var result : FixExt_4_1_IN = {
    e0 = BitInt_IN.init( quantize2int( v[0] ) ),
    e1 = BitInt_IN.init( quantize2int( v[1] ) ),
    e2 = BitInt_IN.init( quantize2int( v[2] ) ),
    e3 = BitInt_IN.init( quantize2int( 1 ) )
  }
  return result
end
local gong function toExt( v : vec3 ) : Ext4_1
  var result : Ext4_1 = {
    e0 = quantize( v[0] ),
    e1 = quantize( v[1] ),
    e2 = quantize( v[2] ),
    e3 = quantize( 1 )
  }
  return result
end


local print41 = G.Macro(function(prefix, x)
  if (gong `x.e0):gettype() == num then return gong quote
    G.print(prefix, x.e0, x.e1, x.e2, x.e3)
  end else return gong quote
    G.print(prefix, x.e0.limbs, x.e1.limbs, x.e2.limbs, x.e3.limbs)
  end end
end)
local print42 = G.Macro(function(prefix, x)
  if (gong `x.e01):gettype() == num then return gong quote
    G.print(prefix, x.e01, x.e02, x.e03, x.e12, x.e13, x.e23)
  end else return gong quote
    G.print(prefix, x.e01.limbs, x.e02.limbs, x.e03.limbs,
                    x.e12.limbs, x.e13.limbs, x.e23.limbs)
  end end
end)
local print43 = G.Macro(function(prefix, x)
  if (gong `x.e012):gettype() == num then return gong quote
    G.print(prefix, x.e012, x.e013, x.e023, x.e123)
  end else return gong quote
    G.print(prefix, x.e012.limbs, x.e013.limbs, x.e023.limbs, x.e123.limbs)
  end end
end)


-------------------------

local COEFF_IT12_PISCT    = 10.0*EPS +  64.0*EPS2
local COEFF_IT12_S1       = 20.0*EPS + 256.0*EPS2
local COEFF_IT12_S2       = 24.0*EPS + 512.0*EPS2
local filterCheck = G.Macro(function(val, absval, coeff)
  return gong `( G.abs(val) > absval*coeff )
end)

local edge_test = G.Macro(function( uncertain, e, ke, p0, p1, kp0, kp1 )
  return gong quote
    var a     = Ext.join( p0, p1 )
    var ka    = AExt.join( kp0, kp1 )
    var dot   = Ext.inner(e, a)
    var kdot  = AExt.inner(ke, ka)
    var outside   = (dot < 0)
    var reliable  = filterCheck(dot, kdot, COEFF_IT12_S1)
    --print42('a ',a)
    --G.print('sign/test e: ', reliable? (outside? -1 else 1) else 0,
    --                         dot)
    if reliable and outside then return 1
    elseif not reliable     then uncertain = true end
  end
end)

local tri_test = G.Macro(function( uncertain, t, kt,
                                   p0, p1, p2, kp0, kp1, kp2 )
  return gong quote
    var a     = Ext.join( Ext.join( p0, p1 ), p2 )
    var ka    = AExt.join( AExt.join( kp0, kp1 ), kp2 )
    var dot   = Ext.inner(t, a)
    var kdot  = AExt.inner(kt, ka)
    var outside   = (dot < 0)
    var reliable  = filterCheck(dot, kdot, COEFF_IT12_S2)
    --G.print('sign/test t: ', reliable? (outside? -1 else 1) else 0,
    --                         dot)
    if reliable and outside then return 1
    elseif not reliable     then uncertain = true end
  end
end)

local gong function is_empty_filter( input : TriEdgeInput )
  -- Convert inputs into these formats
  var ep : Ext4_1[2] = :[i] toExt( :[j] input.e[j,i] )
  var tp : Ext4_1[3] = :[i] toExt( :[j] input.t[j,i] )
  var kep            = :[i] AExt.abs( ep[i] )
  var ktp            = :[i] AExt.abs( tp[i] )
  --print41('    ep0', ep[0])
  --print41('    ep1', ep[1])
  --print41('    tp0', tp[0])
  --print41('    tp1', tp[1])
  --print41('    tp2', tp[2])

  -- form the edge and triangle flats
  var e   = Ext.join(ep[0], ep[1])
  var ke  = AExt.join(kep[0], kep[1])
  var t   = Ext.join( Ext.join(tp[0], tp[1]), tp[2] )
  var kt  = AExt.join( AExt.join(ktp[0], ktp[1]), ktp[2] )

  -- compute the point of intersection
  var pisct   = Ext.meet(e, t)
  var kpisct  = AExt.meet(ke, kt)
  -- FILTER_CHECK
  if not filterCheck( pisct.e3, kpisct.e3, COEFF_IT12_PISCT ) then
    return 0
  elseif pisct.e3 < 0 then
    pisct = Ext.neg(pisct)
  end
  --print41('pisct', pisct)
  --print43('    tri  ', t)
  --print42('    edge ', e)

  var uncertain = false

  edge_test(uncertain, e,ke, pisct, ep[1], kpisct, kep[1])
  edge_test(uncertain, e,ke, ep[0], pisct, kep[0], kpisct)

  tri_test(uncertain, t,kt, pisct, tp[1], tp[2], kpisct, ktp[1], ktp[2])
  tri_test(uncertain, t,kt, tp[0], pisct, tp[2], ktp[0], kpisct, ktp[2])
  tri_test(uncertain, t,kt, tp[0], tp[1], pisct, ktp[0], ktp[1], kpisct)

  if uncertain then return 0
               else return -1 end
end


-------------------------


local LINE_BITS         = 2*IN_BITS + 1
local TRI_BITS          = LINE_BITS + IN_BITS + 2
local ISCT_BITS         = TRI_BITS + LINE_BITS + 2
local LINE_A_BITS       = ISCT_BITS + IN_BITS + 1
local TRI_A_BITS        = LINE_A_BITS + IN_BITS + 2
local INNER_LINE_BITS   = LINE_A_BITS + LINE_BITS + 3
local INNER_TRI_BITS    = TRI_A_BITS + TRI_BITS + 2
local gong function is_empty_exact_fallback( input : TriEdgeInput )
  -- Convert inputs into these formats
  var ep : FixExt_4_1_IN[2] = :[i] toFixExt( :[j] input.e[j,i] )
  var tp : FixExt_4_1_IN[3] = :[i] toFixExt( :[j] input.t[j,i] )
  --
  var R1 = RESHRINK
  var R2, R3 = R1*R1, R1*R1*R1
  var R4, R5, R6 = R2*R2, R2*R3, R3*R3
  var R7, R8, R9, R10 = R3*R4, R4*R4, R4*R5, R5*R5
  --print41('    ep0', ep[0])
  --print41('    ep1', FExt.to_ext(ep[1], R1))
  --print41('    ep1', ep[1])
  --print41('    tp0', tp[0])
  --print41('    tp1', tp[1])
  --print41('    tp2', tp[2])

  -- construct basic flats
  var e = FExt.join( IN_BITS, ep[0], IN_BITS, ep[1] )
  var t = FExt.join( LINE_BITS, FExt.join( IN_BITS, tp[0], 
                                           IN_BITS, tp[1] ),
                     IN_BITS, tp[2] )

  -- the point of intersection, with sign-adjustment for w-coordinate
  var pisct   = FExt.meet( LINE_BITS, e, TRI_BITS, t )
  --print41('Fpisct', pisct)
  --print41('Fpisct', FExt.to_ext(pisct,R5))
  --print42('    Fedge', FExt.to_ext(e, R2))
  --print43('    Ftri ', FExt.to_ext(t, R3))
  var e3sign  = FExt.sign( pisct.e3 )
  if e3sign < 0 then
    pisct     = FExt.neg( ISCT_BITS, pisct )
  elseif e3sign == 0 then
    return DegenerateResult()
  end

  -- does the intersection point lie between the two edge endpoints?
  var ae0     = FExt.join( ISCT_BITS, pisct, IN_BITS, ep[1] )
  var ae1     = FExt.join( IN_BITS, ep[0], ISCT_BITS, pisct )
  var test_e0 = FExt.inner( LINE_BITS, e, LINE_A_BITS, ae0 )
  var test_e1 = FExt.inner( LINE_BITS, e, LINE_A_BITS, ae1 )
  var sign_e0 = FExt.sign( test_e0 )
  var sign_e1 = FExt.sign( test_e1 )
  --print42('ae0 ', ae0)
  --print42('ae0 ', FExt.to_ext(ae0,R6))
  --print42('ae1 ', FExt.to_ext(ae1,R6))
  --G.print('test e: ', test_e0.limbs,test_e1.limbs)
  --G.print('test e: ', FExt.to_ext(test_e0,R8), FExt.to_ext(test_e1,R8))
  --G.print('sign e: ', sign_e0,sign_e1)

  -- is the intersection point contained inside the triangle?
  var at0     = FExt.join( LINE_A_BITS,
                           FExt.join( ISCT_BITS, pisct, IN_BITS, tp[1] ),
                           IN_BITS, tp[2] )
  var at1     = FExt.join( LINE_A_BITS,
                           FExt.join( IN_BITS, tp[0], ISCT_BITS, pisct ),
                           IN_BITS, tp[2] )
  var at2     = FExt.join( LINE_BITS,
                           FExt.join( IN_BITS, tp[0], IN_BITS, tp[1] ),
                           ISCT_BITS, pisct )
  var test_t0 = FExt.inner( TRI_BITS, t, TRI_A_BITS, at0 )
  var test_t1 = FExt.inner( TRI_BITS, t, TRI_A_BITS, at1 )
  var test_t2 = FExt.inner( TRI_BITS, t, TRI_A_BITS, at2 )
  var sign_t0 = FExt.sign(test_t0)
  var sign_t1 = FExt.sign(test_t1)
  var sign_t2 = FExt.sign(test_t2)
  --G.print('test t: ', FExt.to_ext(test_t0,R10),
  --                    FExt.to_ext(test_t1,R10),
  --                    FExt.to_ext(test_t2,R10))
  --G.print('sign t: ', sign_t0,sign_t1,sign_t2)

  if sign_e0 < 0 or sign_e1 < 0 or
     sign_t0 < 0 or sign_t1 < 0 or sign_t2 < 0 then
    return NoIsctResult()
  elseif sign_e0 == 0 or sign_e1 == 0 or
         sign_t0 == 0 or sign_t1 == 0 or sign_t2 == 0 then
    return DegenerateResult()
  else
    return YesIsctResult()
  end
end


-------------------------

local DO_DEBUG = false
local gong function is_empty_exact( input : TriEdgeInput )
  var filter    = is_empty_filter( input )
  if filter == 0 then
    return is_empty_exact_fallback( input )
  elseif filter > 0 then
    if DO_DEBUG then
      var fallback  = is_empty_exact_fallback( input )
      if not result_is_no_isct(fallback) then
        G.print("ERROR EXPECTED 0 FALLBACK, got", fallback.code)
        G.print("    e", input.e)
        G.print("    t", input.t)
        return DegenerateResult()
      end
    end
    return NoIsctResult()
  else
    if DO_DEBUG then
      var fallback  = is_empty_exact_fallback( input )
      if not result_is_isct(fallback) then
        G.print("ERROR EXPECTED 1 FALLBACK, got", fallback.code)
        G.print("    e", input.e)
        G.print("    t", input.t)
        return DegenerateResult()
      end
    end
    return YesIsctResult()
  end
end

local gong function has_common_vert( edge:Edges, tri:Triangles ) : G.bool
  if edge.hd == tri.v[0] or
     edge.hd == tri.v[1] or
     edge.hd == tri.v[2] or
     edge.tl == tri.v[0] or
     edge.tl == tri.v[1] or
     edge.tl == tri.v[2] then   return true
                         else   return false end
end

local gong function find_et_isct( edge:Edges, tri:Triangles ) : IsctResult
  -- If the two primitives share a vertex,
  -- then assuming general position, this vertex is their
  -- (unremarkable) unique point of intersection
  --G.print('try find')
  if has_common_vert(edge, tri) then
    return NoIsctResult()
  end

  -- otherwise, run empty predicate
  var input = { e = { edge.hd.pos, edge.tl.pos },
                t = { tri.v[0].pos, tri.v[1].pos, tri.v[2].pos } }
  return is_empty_exact(input)
end

local gong function boxbox_test( edge : Edges, tri : Triangles )
  var emin = :[i] G.min( edge.hd.pos[i], edge.tl.pos[i] )
  var emax = :[i] G.max( edge.hd.pos[i], edge.tl.pos[i] )
  --G.print('eminmax',emin, emax)
  --G.print('t', tri.v[0].pos, tri.v[1].pos, tri.v[2].pos)

  for k=0,3 do
    if (tri.v[0].pos[k] < emin[k] and tri.v[1].pos[k] < emin[k]
                                  and tri.v[2].pos[k] < emin[k])
    or (tri.v[0].pos[k] > emax[k] and tri.v[1].pos[k] > emax[k]
                                  and tri.v[2].pos[k] > emax[k])
    then return false end
  end

  return true
end

------------------------------------------------------------------------------
-- Join

local gong join find_et_iscts ( edge : Edges, tri : Triangles )
  where boxbox_test(edge,tri)
  --G.print('HIT1')
  --G.print(edge.hd.pos, ';', edge.tl.pos)
  --G.print(tri.v[0].pos, ';', tri.v[1].pos, ';', tri.v[2].pos)
  var isct_result = find_et_isct(edge,tri)
  where not result_is_no_isct( isct_result )
do
  if result_is_isct( isct_result ) then
    --G.print('code', isct_result.code)
    --G.print('edge', edge.tl.pos, edge.hd.pos)
    --G.print('tri ', tri.v[0].pos)
    emit { edge=edge, tri=tri } in ET_Iscts
  else -- degeneracy
    has_degeneracies or= true
    degeneracy_count += 1
  end
end

local gong join has_et_iscts ( edge : Edges, tri : Triangles )
  where boxbox_test(edge,tri)
  var isct_result = find_et_isct(edge,tri)
  where not result_is_no_isct( isct_result )
do
  if result_is_isct( isct_result ) then
    has_intersections or= true
  else -- is degenerate
    has_degeneracies or= true
  end
end


------------------------------------------------------------------------------
-- Join

local gong function Tris_to_AABB3f( t : Triangles ) : G.AABB3f
  return { lo = :[i] G.float(:min[j] t.v[j].pos[i] - FLT_EPSILON),
           hi = :[i] G.float(:max[j] t.v[j].pos[i] + FLT_EPSILON) }
end

local gong function Edges_to_AABB3f( e : Edges ) : G.AABB3f
  return { lo = :[i] G.float(G.min( e.hd.pos[i], e.tl.pos[i] ) -FLT_EPSILON),
           hi = :[i] G.float(G.max( e.hd.pos[i], e.tl.pos[i] ) +FLT_EPSILON) }
end

local Tri_BVH = G.bvh_index {
  table       = Triangles,
  volume      = G.AABB3f,
  abstract    = Tris_to_AABB3f,
  vol_union   = G.AABB3f_union,
  point       = G.AABB3f_midpoint,
}

local Edge_BVH = G.bvh_index {
  table       = Edges,
  volume      = G.AABB3f,
  abstract    = Edges_to_AABB3f,
  vol_union   = G.AABB3f_union,
  point       = G.AABB3f_midpoint,
}

local BVH_Traversal = G.bvh_bvh_traversal {
  left        = Edge_BVH,
  right       = Tri_BVH,
  vol_isct    = G.AABB3f_isct,
}

find_et_iscts:set_cpu_traversal(BVH_Traversal)
has_et_iscts:set_cpu_traversal(BVH_Traversal)


------------------------------------------------------------------------------
-- Profiling & Debugging Help

if BUFFER_EFF then
  find_et_iscts:buffer_effects_on_cpu(true)
  has_et_iscts:buffer_effects_on_cpu(true)
end
if BUFFER_IDX then
  find_et_iscts:buffer_index_on_cpu(true)
  has_et_iscts:buffer_index_on_cpu(true)
end
if VERIFY_IDX then
  find_et_iscts:verify_index(true)
  has_et_iscts:verify_index(true)
end

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Export

local API = G.CompileLibrary {
  tables          = {Vertices, Edges, Triangles},
  joins           = {find_et_iscts, has_et_iscts},
  c_obj_file      = 'cork_isct.o',
  cpp_header_file = 'cork_isct.h',
}




