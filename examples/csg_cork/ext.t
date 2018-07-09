import 'gong'
local G = gong.stdlib

local Exports   = {}
package.loaded["ext"] = Exports

local prelude   = require 'prelude'
local num       = prelude.num

------------------------------------------------------------------------------
-- Ext is an exterior calculus system
-- AExt is similar, but computes absolute-value bounds on Ext computations

local Ext4_1 = G.record { { 'e0', num },
                          { 'e1', num },
                          { 'e2', num },
                          { 'e3', num } }
local Ext4_2 = G.record { { 'e01', num },
                          { 'e02', num },
                          { 'e03', num },
                          { 'e12', num },
                          { 'e13', num },
                          { 'e23', num } }
local Ext4_3 = G.record { { 'e012', num },
                          { 'e013', num },
                          { 'e023', num },
                          { 'e123', num } }
Exports.Ext4_1 = Ext4_1
Exports.Ext4_2 = Ext4_2
Exports.Ext4_3 = Ext4_3

-- because of structural typing, it'll end up being the same
Exports.AExt4_1 = Ext4_1
Exports.AExt4_2 = Ext4_2
Exports.AExt4_3 = Ext4_3


------------------------------------------------------------------------------

local function Ext(dim)
  if      dim == 1 then   return Ext4_1
  elseif  dim == 2 then   return Ext4_2
  elseif  dim == 3 then   return Ext4_3
  else assert('UNEXPECTED CALL with dim: '..dim) end
end
local function getdim( typ )
  assert( typ:is_record() )
  if      typ == Ext4_1 then    return 1
  elseif  typ == Ext4_2 then    return 2
  elseif  typ == Ext4_3 then    return 3
  else assert('UNEXPECTED CALL with type: '..tostring(typ)) end
end


------------------------------------------------------------------------------
-- Ext methods

Exports.Ext   = {}


local gong function neg_E1( x : Ext4_1 ) : Ext4_1
  var r : Ext4_1 = {
    e0 = -x.e0,
    e1 = -x.e1,
    e2 = -x.e2,
    e3 = -x.e3
  }
  return r
end
local gong function neg_E2( x : Ext4_2 ) : Ext4_2
  var r : Ext4_2 = {
    e01 = -x.e01,
    e02 = -x.e02,
    e03 = -x.e03,
    e12 = -x.e12,
    e13 = -x.e13,
    e23 = -x.e23
  }
  return r
end
local gong function neg_E3( x : Ext4_3 ) : Ext4_3
  var r : Ext4_3 = {
    e012 = -x.e012,
    e013 = -x.e013,
    e023 = -x.e023,
    e123 = -x.e123
  }
  return r
end
local neg = G.Macro( function( x )
  local xRep  = x:gettype()
  local negf  = ({ neg_E1, neg_E2, neg_E3 })[getdim(xRep)]
  return gong `negf(x)
end)
Exports.Ext.neg   = neg

------------------------------

local gong function dual_E1( x : Ext4_1 ) : Ext4_3
  var r : Ext4_3 = {
    e012 =  x.e3,
    e013 = -x.e2,
    e023 =  x.e1,
    e123 = -x.e0
  }
  return r
end
local gong function dual_E2( x : Ext4_2 ) : Ext4_2
  var r : Ext4_2 = {
    e01 =  x.e23,
    e02 = -x.e13,
    e03 =  x.e12,
    e12 =  x.e03,
    e13 = -x.e02,
    e23 =  x.e01
  }
  return r
end
local gong function dual_E3( x : Ext4_3 ) : Ext4_1
  var r : Ext4_1 = {
    e0 =  x.e123,
    e1 = -x.e023,
    e2 =  x.e013,
    e3 = -x.e012
  }
  return r
end
local dual = G.Macro( function( x )
  local xRep  = x:gettype()
  local dualf  = ({ dual_E1, dual_E2, dual_E3 })[getdim(xRep)]
  return gong `dualf(x)
end)
Exports.Ext.dual   = dual

------------------------------

local gong function revdual_E1( x : Ext4_1 ) : Ext4_3
  var r : Ext4_3 = {
    e012 = -x.e3,
    e013 =  x.e2,
    e023 = -x.e1,
    e123 =  x.e0
  }
  return r
end
local gong function revdual_E2( x : Ext4_2 ) : Ext4_2
  var r : Ext4_2 = {
    e01 =  x.e23,
    e02 = -x.e13,
    e03 =  x.e12,
    e12 =  x.e03,
    e13 = -x.e02,
    e23 =  x.e01
  }
  return r
end
local gong function revdual_E3( x : Ext4_3 ) : Ext4_1
  var r : Ext4_1 = {
    e0 = -x.e123,
    e1 =  x.e023,
    e2 = -x.e013,
    e3 =  x.e012
  }
  return r
end
local revdual = G.Macro( function( x )
  local xRep  = x:gettype()
  local revdualf  = ({ revdual_E1, revdual_E2, revdual_E3 })[getdim(xRep)]
  return gong `revdualf(x)
end)
Exports.Ext.revdual   = revdual

------------------------------

local gong function join_E1_E1( x : Ext4_1, y : Ext4_1 ) : Ext4_2
  var r : Ext4_2 = {
    e01 =  x.e0 * y.e1 - y.e0 * x.e1,
    e02 =  x.e0 * y.e2 - y.e0 * x.e2,
    e03 =  x.e0 * y.e3 - y.e0 * x.e3,
    e12 =  x.e1 * y.e2 - y.e1 * x.e2,
    e13 =  x.e1 * y.e3 - y.e1 * x.e3,
    e23 =  x.e2 * y.e3 - y.e2 * x.e3
  }
  return r
end
local gong function join_E2_E1( x : Ext4_2, y : Ext4_1 ) : Ext4_3
  var r : Ext4_3 = {
    e012 =  x.e01 * y.e2 - x.e02 * y.e1 + x.e12 * y.e0,
    e013 =  x.e01 * y.e3 - x.e03 * y.e1 + x.e13 * y.e0,
    e023 =  x.e02 * y.e3 - x.e03 * y.e2 + x.e23 * y.e0,
    e123 =  x.e12 * y.e3 - x.e13 * y.e2 + x.e23 * y.e1
  }
  return r
end
local join = G.Macro( function( x, y )
  local xRep  = x:gettype()
  local yRep  = y:gettype()
  if yRep == Ext4_2 then
    x,y, xRep,yRep  = y,x, yRep,xRep
  end
  assert( yRep == Ext4_1, 'bad join type args' )
  local joinf  = ({ join_E1_E1, join_E2_E1 })[getdim(xRep)]
  return gong `joinf(x,y)
end)
Exports.Ext.join   = join

------------------------------

local gong function meet_E3_E3( x : Ext4_3, y : Ext4_3 ) : Ext4_2
  return revdual( join( dual(x), dual(y) ) )
end
local gong function meet_E2_E3( x : Ext4_2, y : Ext4_3 ) : Ext4_1
  return revdual( join( dual(x), dual(y) ) )
end
local meet = G.Macro( function( x, y )
  local xRep  = x:gettype()
  local yRep  = y:gettype()
  if yRep == Ext4_2 then
    x,y, xRep,yRep  = y,x, yRep,xRep
  end
  assert( yRep == Ext4_3, 'bad meet type args' )
  local meetf  = ({ 0, meet_E2_E3, meet_E3_E3 })[getdim(xRep)]
  return gong `meetf(x,y)
end)
Exports.Ext.meet   = meet

------------------------------

local gong function inner_E1( x : Ext4_1, y : Ext4_1 ) : num
  return x.e0 * y.e0
       + x.e1 * y.e1
       + x.e2 * y.e2
       + x.e3 * y.e3
end
local gong function inner_E2( x : Ext4_2, y : Ext4_2 ) : num
  return x.e01 * y.e01
       + x.e02 * y.e02
       + x.e03 * y.e03
       + x.e12 * y.e12
       + x.e13 * y.e13
       + x.e23 * y.e23
end
local gong function inner_E3( x : Ext4_3, y : Ext4_3 ) : num
  return x.e012 * y.e012
       + x.e013 * y.e013
       + x.e023 * y.e023
       + x.e123 * y.e123
end
local inner = G.Macro( function( x, y )
  local xRep  = x:gettype()
  assert( y:gettype() == xRep, 'expect matching arg types' )
  local innerf  = ({ inner_E1, inner_E2, inner_E3 })[getdim(xRep)]
  return gong `innerf(x,y)
end)
Exports.Ext.inner   = inner

------------------------------


------------------------------------------------------------------------------
-- Ext methods

Exports.AExt  = {}


local gong function abs_E1( x : Ext4_1 ) : Ext4_1
  var r : Ext4_1 = {
    e0 = G.abs( x.e0 ),
    e1 = G.abs( x.e1 ),
    e2 = G.abs( x.e2 ),
    e3 = G.abs( x.e3 )
  }
  return r
end
local gong function abs_E2( x : Ext4_2 ) : Ext4_2
  var r : Ext4_2 = {
    e01 = G.abs( x.e01 ),
    e02 = G.abs( x.e02 ),
    e03 = G.abs( x.e03 ),
    e12 = G.abs( x.e12 ),
    e13 = G.abs( x.e13 ),
    e23 = G.abs( x.e23 )
  }
  return r
end
local gong function abs_E3( x : Ext4_3 ) : Ext4_3
  var r : Ext4_3 = {
    e012 = G.abs( x.e012 ),
    e013 = G.abs( x.e013 ),
    e023 = G.abs( x.e023 ),
    e123 = G.abs( x.e123 )
  }
  return r
end
local abs = G.Macro( function( x )
  local xRep  = x:gettype()
  local absf  = ({ abs_E1, abs_E2, abs_E3 })[getdim(xRep)]
  return gong `absf(x)
end)
Exports.AExt.abs = abs

------------------------------

local absneg = G.Macro( function( x ) return x end )
Exports.AExt.neg  = absneg

local gong function absdual_E1( x : Ext4_1 ) : Ext4_3
  var r : Ext4_3 = {
    e012 = x.e3,
    e013 = x.e2,
    e023 = x.e1,
    e123 = x.e0
  }
  return r
end
local gong function absdual_E2( x : Ext4_2 ) : Ext4_2
  var r : Ext4_2 = {
    e01 = x.e23,
    e02 = x.e13,
    e03 = x.e12,
    e12 = x.e03,
    e13 = x.e02,
    e23 = x.e01
  }
  return r
end
local gong function absdual_E3( x : Ext4_3 ) : Ext4_1
  var r : Ext4_1 = {
    e0 = x.e123,
    e1 = x.e023,
    e2 = x.e013,
    e3 = x.e012
  }
  return r
end
local absdual = G.Macro( function( x )
  local xRep      = x:gettype()
  local absdualf  = ({ absdual_E1, absdual_E2, absdual_E3 })[getdim(xRep)]
  return gong `absdualf(x)
end)
Exports.AExt.dual     = absdual
Exports.AExt.revdual  = absdual

------------------------------

local gong function absjoin_E1_E1( x : Ext4_1, y : Ext4_1 ) : Ext4_2
  var r : Ext4_2 = {
    e01 =  x.e0 * y.e1 + y.e0 * x.e1,
    e02 =  x.e0 * y.e2 + y.e0 * x.e2,
    e03 =  x.e0 * y.e3 + y.e0 * x.e3,
    e12 =  x.e1 * y.e2 + y.e1 * x.e2,
    e13 =  x.e1 * y.e3 + y.e1 * x.e3,
    e23 =  x.e2 * y.e3 + y.e2 * x.e3
  }
  return r
end
local gong function absjoin_E2_E1( x : Ext4_2, y : Ext4_1 ) : Ext4_3
  var r : Ext4_3 = {
    e012 =  x.e01 * y.e2 + x.e02 * y.e1 + x.e12 * y.e0,
    e013 =  x.e01 * y.e3 + x.e03 * y.e1 + x.e13 * y.e0,
    e023 =  x.e02 * y.e3 + x.e03 * y.e2 + x.e23 * y.e0,
    e123 =  x.e12 * y.e3 + x.e13 * y.e2 + x.e23 * y.e1
  }
  return r
end
local absjoin = G.Macro( function( x, y )
  local xRep  = x:gettype()
  local yRep  = y:gettype()
  if yRep == Ext4_2 then
    x,y, xRep,yRep  = y,x, yRep,xRep
  end
  assert( yRep == Ext4_1, 'bad absjoin type args' )
  local absjoinf  = ({ absjoin_E1_E1, absjoin_E2_E1 })[getdim(xRep)]
  return gong `absjoinf(x,y)
end)
Exports.AExt.join      = absjoin

------------------------------

local meet = G.Macro( function( x, y )
  local xRep  = x:gettype()
  local yRep  = y:gettype()
  if yRep == Ext4_2 then
    x,y, xRep,yRep  = y,x, yRep,xRep
  end
  assert( yRep == Ext4_3, 'bad meet type args' )
  local meetf  = ({ 0, meet_E2_E3, meet_E3_E3 })[getdim(xRep)]
  return gong `meetf(x,y)
end)

local gong function absmeet_E3_E3( x : Ext4_3, y : Ext4_3 ) : Ext4_2
  return revdual( join( dual(x), dual(y) ) )
end
local gong function absmeet_E2_E3( x : Ext4_2, y : Ext4_3 ) : Ext4_1
  return revdual( join( dual(x), dual(y) ) )
end
local absmeet = G.Macro( function( x, y )
  local xRep  = x:gettype()
  local yRep  = y:gettype()
  if yRep == Ext4_2 then
    x,y, xRep,yRep  = y,x, yRep,xRep
  end
  assert( yRep == Ext4_3, 'bad absmeet type args' )
  local absmeetf  = ({ 0, absmeet_E2_E3, absmeet_E3_E3 })[getdim(xRep)]
  return gong `absmeetf(x,y)
end)
Exports.AExt.meet     = absmeet

------------------------------

Exports.AExt.inner    = inner



