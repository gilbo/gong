import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib
local E     = (require 'gong.src.effectcheck').Effects
local Util  = require 'gong.src.util'

------------------------------------------------------------------------------

local function ins(tbl, idx)
  local t     = tbl[idx]
  if t == nil then t = {}; tbl[idx] = t end
  return t
end

local BOTTOM = {}
local function test_eff_eq(fobj, template)
  assert(terralib.israwlist(template))
  local effs        = fobj:_INTERNAL_geteffects()
  local errs        = terralib.newlist()

  if #template ~= #effs then
    errs:insert('expected '..#template..' effects but got '..#effs) end

  -- copy of the template, structured
  local cp = {}
  for _,e in ipairs(template) do
    local ep    = ins(cp, e.effect)
    --
    if e.type then
      ep        = ins(ep, e.type)
    end
    --
    if e.path then
      for _,tkn in ipairs(e.path) do
        ep      = ins(ep, tkn) 
      end
    end
    --
    if not ep[BOTTOM] then ep[BOTTOM] = terralib.newlist() end
    ep[BOTTOM]:insert(e)
  end

  -- now search for effects
  for _,e in ipairs(effs) do
    local lookup    = terralib.newlist()

    if      E.Filter.check(e) then      lookup:insert('filter')
    elseif  E.Scan.check(e) then        lookup:insert('scan')
                                        lookup:insert(e.src)
    elseif  E.Emit.check(e) then        lookup:insert('emit')
                                        lookup:insert(e.dst)
    elseif  E.Return.check(e) then      lookup:insert('return')
                                        lookup:insert(e.type)
    elseif  E.Write.check(e) then       lookup:insert('write')
                                        lookup:insert(e.dst)
    elseif  E.Reduce.check(e) then      lookup:insert('reduce('..e.op..')')
                                        lookup:insert(e.dst)
    elseif  E.Read.check(e) then        lookup:insert('read')
                                        lookup:insert(e.src)
    else error('UNRECOGNIZED EFFECT') end
    for _,p in ipairs(e.path or {}) do
      if     E.FieldToken.check(p) then   lookup:insert(p.name)
      elseif E.IndexToken.check(p) then   lookup:insert('#')
      else error('UNRECOGNIZED TOKEN') end
    end

    -- do lookup
    local find  = cp
    for _,tkn in ipairs(lookup) do
      find = find[tkn]
      if find == nil then break end
    end
    if find and find[BOTTOM] and #find[BOTTOM] > 0 then
      find[BOTTOM]:remove() -- pop
    else
      errs:insert('  Unexpected Effect: '..tostring(e))
    end
  end

  -- read out all the remaining expected effects
  local function rem(tbl)
    if tbl[BOTTOM] then
      for _,e in ipairs(tbl[BOTTOM]) do
        errs:insert('  Expected Effect:   '..
                    string.format('%-10s%-20s%s', 
                      e.effect,
                      e.type or '',
                      (e.path and table.concat(e.path, '.')) or ''
                    ))
      end
    else
      for _,t in pairs(tbl) do rem(t) end
    end
  end
  rem(cp)

  if #errs > 0 then
    error('\n'..errs:concat('\n'), 2)
  end
end


------------------------------------------------------------------------------

gong function retzero(a : G.int32, b : G.int32) : G.int32
  return 0
end
test_eff_eq(retzero, {})

gong function retpair()
  return 0, 1
end
test_eff_eq(retpair, {})

local gong function domath(x : G.float)
  var y = x * x
  return 1.0f * y + 32f * -x + 12.0f
end
test_eff_eq(domath, {})

local gong function doblock()
  var x = 1
  do
    x = x + x
    return x
  end
end
test_eff_eq(doblock, {})


------------------------------------------------------------------------------

local recT = G.record { {'f1',double}, {'f2',double} }
local RowObjs = G.NewTable('RowObjs')
RowObjs:NewField('val', G.double)
local gong function swapfields( obj : recT, row : RowObjs )
  var temp = obj['f1']
  obj.f1 = obj.f2
  obj['f2'] = temp
  return row.val + temp
end
test_eff_eq(swapfields, {
  {effect='read', type=G.row(RowObjs), path={'val'}}
})

test.fail(function()
  local gong function swapfields( obj : recT, row : RowObjs )
    row.val   = obj.f1
    return 0
  end
end, 'writing to table fields is not allowed')


------------------------------------------------------------------------------

local ValS = G.record{ {'s',G.double},{'t',G.int32} }

local A = G.NewTable('A')
local B = G.NewTable('B')
local C = G.NewTable('C')
A:NewField('id', G.int32)
A:NewField('val', ValS)
B:NewField('id', G.int32)
B:NewField('val', G.double)
C:NewField('a', A)
C:NewField('b', B)
local gong join aboff( a : A, b : B )
  where a.id+1 == b.id
do
  emit { a=a, b=b } in C
end
test_eff_eq(aboff, {
  {effect='scan', type=G.row(A)},
  {effect='scan', type=G.row(B)},
  {effect='read', type=G.row(A), path={'id'}},
  {effect='read', type=G.row(B), path={'id'}},
  {effect='filter'},
  {effect='emit', type=G.row(C)},
})



local gong join redjoin( a : A, b : B )
  where a.id+1 == b.id
do
  a.val.s += b.val
end
test_eff_eq(redjoin, {
  {effect='scan', type=G.row(A)},
  {effect='scan', type=G.row(B)},
  {effect='read', type=G.row(A), path={'id'}},
  {effect='read', type=G.row(B), path={'id'}},
  {effect='filter'},
  {effect='reduce(+)', type=G.row(A), path={'val','s'}},
  {effect='read', type=G.row(B), path={'val'}},
})

test.fail(function()
  local gong join redjoin( a : A, b : B )
    where a.id+1 == b.id
  do
    emit { id=b.id, val={s=b.val,t=a.val.t} } in A
  end
end, 'joins must emit into a different table than the operands of')


------------------------------------------------------------------------------


