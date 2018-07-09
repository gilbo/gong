local test  = require 'tests.test'

local T     = require 'gong.src.types'


---------------------------------------
-- Test creation of various AST nodes 

-- dummy test
test.eq(T.is_type('foo'), false)

-- test that a few primitive types are present
test.eq(T.is_type(T.int32),  true)
test.eq(T.is_type(T.uint64), true)
test.eq(T.is_type(T.bool),   true)
test.eq(T.is_type(T.float),  true)
test.eq(T.is_type(T.double), true)

local prims = {
  ['int8'  ] = int8,
  ['int16' ] = int16,
  ['int32' ] = int32,
  ['int64' ] = int64,
  ['uint8' ] = uint8,
  ['uint16'] = uint16,
  ['uint32'] = uint32,
  ['uint64'] = uint64,
  ['size8' ] = uint8,
  ['size16'] = uint16,
  ['size32'] = uint32,
  ['size64'] = uint64,
  ['bool'  ] = bool,
  ['float' ] = float,
  ['double'] = double,
}

for name,terra_type in pairs(prims) do
  -- test that type is there
  local typ = T[name]
  test.eq(T.is_type(typ), true)

  -- properties of type
  test.eq(typ:is_primitive(), true)
  test.eq(typ:is_value(), true)
  test.eq(typ:is_record(), false)
  test.eq(typ:is_tensor(), false)

  -- function to check a type for tensor-independent properties
  local function tensorindependenttests(sometyp)
    -- check various sub-properties
    if name == 'bool' then
      test.eq(sometyp:is_logical(), true)
      test.eq(sometyp:is_numeric(), false)
      test.eq(sometyp:is_integral(), false)
      test.eq(sometyp:is_float(), false)
    else
      test.eq(sometyp:is_logical(), false)
      test.eq(sometyp:is_numeric(), true)
      local is_integral  = name:sub(1,3) == 'int' or
                           name:sub(1,4) == 'uint' or
                           name:sub(1,4) == 'size'
      test.eq(sometyp:is_integral(), is_integral)
      local is_float     = not is_integral
      test.eq(sometyp:is_float(), is_float)
    end
  end
  tensorindependenttests(typ)

  -- check simple derivations
  test.eq(typ:terratype(), terra_type)
  test.eq(tostring(typ), name)

  -- check coercion identities
  test.eq(typ:is_coercable_to(typ), true)
  test.eq(typ:coerce_to(typ), typ)
  test.eq(typ:join(typ), typ)

  -- construct some vector, matrix and tensor types and test them
  for dim1 = 2,4 do
    local vectyp = T.vector(typ, dim1)
    test.eq(vectyp, T.tensor(typ, dim1))
    if typ == T.float or typ == T.double then
      -- test that it's the same as the shorthand
      local shortname = 'vec'..tostring(dim1)
      if typ == T.float then shortname = shortname..'f'
                        else shortname = shortname..'d' end
      test.eq(vectyp, T[shortname])
    end

    -- test some properties
    test.eq(vectyp:is_primitive(), false)
    test.eq(vectyp:is_value(), true)
    test.eq(vectyp:is_tensor(), true)
    test.eq(vectyp:is_pure_tensor(), true)

    -- check other properties and derivations
    tensorindependenttests(vectyp)
    test.eq(vectyp:basetype(), typ)
    test.neq(vectyp:terrabasetype(), vectyp:terratype())
    test.eq(tostring(vectyp),
      'tensor('..tostring(typ)..','..tostring(dim1)..')')

    -- check coercion identities
    test.eq(vectyp:is_coercable_to(vectyp), true)
    test.eq(vectyp:coerce_to(vectyp), vectyp)
    test.eq(vectyp:join(vectyp), vectyp)

    for dim2 = 2,4 do
      local mattyp = T.matrix(typ, dim1, dim2)
      test.eq(mattyp, T.tensor(typ, dim1, dim2))
      if typ == T.float or typ == T.double then
        -- test that it's the same as the shorthand
        local shortname = 'mat'..tostring(dim1)..'x'..tostring(dim2)
        if typ == T.float then shortname = shortname..'f'
                          else shortname = shortname..'d' end
        test.eq(mattyp, T[shortname])

        if dim1 == dim2 then
          local symname = 'mat'..tostring(dim1)
          if typ == T.float then symname = symname..'f'
                            else symname = symname..'d' end
          test.eq(mattyp, T[symname])
        end
      end

      -- test some properties
      test.eq(mattyp:is_primitive(), false)
      test.eq(mattyp:is_value(), true)
      test.eq(mattyp:is_tensor(), true)
      test.eq(mattyp:is_pure_tensor(), true)

      -- check other properties and derivations
      tensorindependenttests(mattyp)
      test.eq(mattyp:basetype(), typ)
      test.neq(mattyp:terrabasetype(), mattyp:terratype())
      test.eq(tostring(mattyp),
        'tensor('..tostring(typ)..','..tostring(dim1)..
                                  ','..tostring(dim2)..')')

      -- check coercion identities
      test.eq(mattyp:is_coercable_to(mattyp), true)
      test.eq(mattyp:coerce_to(mattyp), mattyp)
      test.eq(mattyp:join(mattyp), mattyp)

    end
  end
end

-- check a couple of select coercion identities
test.eq(T.float:is_coercable_to(T.double), true)
test.eq(T.float:is_coercable_to(T.int32), false)
test.eq(T.double:is_coercable_to(T.uint64), false)
test.eq(T.int32:is_coercable_to(T.double), true)
test.eq(T.int32:is_coercable_to(T.bool), false)
test.eq(T.uint32:is_coercable_to(T.int64), false)

test.eq(T.float:join(T.int32), T.double) -- controversial
test.eq(T.int32:join(T.uint64), T.error)
test.eq(T.uint32:join(T.int64), T.error)
test.eq(T.uint64:join(T.int32), T.error)
test.eq(T.int64:join(T.uint32), T.error)
test.eq(T.uint64:coerce_to(T.int32), T.error)

test.eq(T.vec3d:coerce_to(T.mat3d), T.error)
test.eq(T.double:join(T.vec4d), T.error) -- could be debated
test.eq(T.vec2f:coerce_to(T.vec2d), T.vec2d)
test.eq(T.mat2x4f:join(T.matrix(T.int32,2,4)), T.mat2x4d)
test.eq(T.error:is_coercable_to(T.double), false)



-- smattering of string tests
test.eq(tostring(T.error), 'error')
test.eq(tostring(T.internal('foo')), 'internal()')

local t222d = T.tensor(T.double,2,2,2)
test.eq(tostring(t222d),'tensor(double,2,2,2)')
test.eq(t222d:is_tensor(), true)


-- weird tests for error and internal
test.eq(T.error:terratype(), T.internal({}):terratype()) -- both {}
-- test known failures for these
test.fail(function()
  T.error:basetype()
end, 'basetype%(%) expected tensor')
test.fail(function()
  T.internal(23):basetype()
end, 'basetype%(%) expected tensor')

-- bad tensor construction
test.fail(function()
  T.tensor(23)
end, 'expected a type')
test.fail(function()
  T.tensor(T.int32, -3)
end, 'expected positive integer size')
test.fail(function()
  T.tensor(T.int32, 0)
end, 'expected positive integer size')


-- lua <--> terra conversions
test.eq(T.terra_to_luaval(T.lua_to_terraval(42, T.int32), T.int32), 42)
test.eq(T.terra_to_luaval(T.lua_to_terraval(42, T.float), T.float), 42)
test.eq(T.terra_to_luaval(T.lua_to_terraval(42, T.double), T.double), 42)
test.eq(T.terra_to_luaval(T.lua_to_terraval(42, T.uint64), T.uint64), 42ULL)

test.eq(T.terra_to_luaval(T.lua_to_terraval(true, T.bool), T.bool), true)
test.eq(T.terra_to_luaval(T.lua_to_terraval(false, T.bool), T.bool), false)
-- weird edge case for booleans...
local btval = terralib.new(bool, true)
local bfval = terralib.new(bool, false)
test.eq(T.terra_to_luaval(btval, T.bool), true)
test.eq(T.terra_to_luaval(bfval, T.bool), false)

local idm = {{1,0},{0,1}}
test.rec_aeq(T.terra_to_luaval(T.lua_to_terraval(idm,T.mat2f), T.mat2f), idm)
test.rec_aeq(T.terra_to_luaval(T.lua_to_terraval(idm,T.mat2d), T.mat2d), idm)

test.eq(T.check_luaval(idm, T.mat2f), true)
test.eq(T.check_luaval(idm, T.mat2d), true)
test.eq(T.check_luaval(idm, T.mat3f), false)
test.eq(T.check_luaval(idm, T.vec4f), false)

test.eq(T.check_luaval(1, T.bool), false)
test.eq(T.check_luaval(true, T.int32), false)
test.eq(T.check_luaval(true, T.error), false)


-- Record Type Tests

local isct3f = T.record {
  { 'v',      T.vec3f },
  { 'is_hit', T.bool  },
}
test.eq(tostring(isct3f), '{ v:tensor(float,3), is_hit:bool }')
test.eq(#isct3f.fields, 2)
test.eq(isct3f.fields[1].name, 'v')
test.eq(isct3f.fields[2].name, 'is_hit')
test.eq(isct3f.fields[1].type, T.vec3f)
test.eq(isct3f.fields[2].type, T.bool)

test.eq(isct3f:is_primitive(), false)
test.eq(isct3f:is_value(), true)
test.eq(isct3f:is_tensor(), false)
test.eq(isct3f:is_pure_tensor(), false)

test.eq(isct3f:terratype():isstruct(), true)
local round = T.terra_to_luaval(
                  T.lua_to_terraval({v={1,2,3},is_hit=true},isct3f),
                  isct3f)
test.eq(type(round), 'table')
test.aeq(round.v, {1,2,3})
test.eq(round.is_hit, true)

test.eq(isct3f:is_coercable_to(isct3f), true)
test.eq(isct3f:coerce_to(isct3f), isct3f)
test.eq(isct3f:join(isct3f), isct3f)


local hit4 = isct3f[4]
test.eq(tostring(hit4), 'tensor({ v:tensor(float,3), is_hit:bool },4)')
test.eq(hit4:is_primitive(), false)
test.eq(hit4:is_value(), true)
test.eq(hit4:is_tensor(), true)
test.eq(hit4:is_pure_tensor(), false)

test.eq(#hit4.dims, 1)
test.eq(hit4.dims[1], 4)
test.eq(hit4:basetype(), isct3f)
test.eq(hit4:terrabasetype(), isct3f:terratype())

test.eq(isct3f:terratype():isstruct(), true)
local orighit = { {v={1,2,3}, is_hit=true},
                  {v={4,5,6}, is_hit=true},
                  {v={7,8,9}, is_hit=true},
                  {v={0,0,0}, is_hit=false},
                }
local hittrip = T.terra_to_luaval(T.lua_to_terraval(orighit,hit4),hit4)
test.eq(type(hittrip), 'table')
test.eq(#hittrip, 4)
for k=1,4 do
  test.eq(type(hittrip[k]),'table')
  test.aeq(hittrip[k].v, orighit[k].v)
  test.eq(hittrip[k].is_hit, orighit[k].is_hit)
end

test.eq(hit4:is_coercable_to(hit4), true)
test.eq(hit4:coerce_to(hit4), hit4)
test.eq(hit4:join(hit4), hit4)


--error('NEED RECORD TYPE TESTS')






