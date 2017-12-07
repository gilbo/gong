import 'gong.src.adt'

local Exports = {}
package.loaded["gong.src.types"] = Exports
local T = Exports

local Util        = require 'gong.src.util'
--local SrcInfo     = Util.SrcInfo
local is_id_str   = Util.is_id_str
local is_pos_int  = Util.is_pos_int
local memoize     = Util.memoize
local memoize_from = Util.memoize_from
local memolist    = Util.memolist

local S           = require 'gong.src.schemata'
-- don't unpack include here, in order to break cyclic dependencies

local newlist     = terralib.newlist
local israwlist   = terralib.israwlist


-------------------------------------------------------------------------------
--[[                          Forward Definitions                          ]]--
-------------------------------------------------------------------------------

local lift_type_from_lua

-------------------------------------------------------------------------------
--[[                            Type Prototype                             ]]--
-------------------------------------------------------------------------------

local Type    = {}
--Type.__index  = Type
-- going to set index to special behavior below

local function NewType(kindname)
  return setmetatable({ _kind = kindname }, Type)
end

local function is_type(obj)
  return getmetatable(obj) == Type
end
Exports.is_type = is_type


-------------------------------------------------------------------------------
--[[                   Primitives and Type Constructors                    ]]--
-------------------------------------------------------------------------------

-- lift some of Terra's primitive types
local primitive_names = {
  "int8",   "uint8",    "size8", 
  "int16",  "uint16",   "size16",
  "int32",  "uint32",   "size32",
  "int64",  "uint64",   "size64",
  --
  "bool",   "float",    "double",
}
local primitives = {}
local terra_to_primitive_table = {}
for i,pname in ipairs(primitive_names) do
  local ptype             = NewType('primitive')
  local is_size           = string.sub(pname,1,4) == 'size'
  ptype._terra_type       = is_size and _G[pname:gsub('size','uint')]
                                     or _G[pname]
  ptype._name             = pname
  ptype._is_size          = is_size

  assert(terralib.types.istype(ptype._terra_type))

  primitives[i]           = ptype
  if not is_size then
    terra_to_primitive_table[ptype._terra_type] = ptype
  end

  Exports[pname] = ptype  -- EXPOSE FROM THE MODULE
end

local function fromterratype(ttype)
  return terra_to_primitive_table[ttype]
end
Exports.fromterratype = fromterratype

-------------------------------------------------------------------------------
-- Error and Internal Types

-- We also add a few types for internal use in the compiler
-- These types should not be exposed to the programmer
Exports.error         = NewType('error')
Exports.error._name   = 'error'

-- The internal type is used to convert arbitrary values into unique types
local function internal(obj)
  local newtyp = NewType('internal')
  newtyp.value = obj
  newtyp._name = 'internal()'
  return newtyp
end
internal = memoize(internal)
Exports.internal = internal

-------------------------------------------------------------------------------
-- Row Types

local function row(table_obj)
  if not S.is_table(table_obj) then
    error('expected Gong Table arg', 2) end

  local rowtyp    = NewType('row')
  rowtyp._table   = table_obj
  rowtyp._name    = 'row('..table_obj:Name()..')'

  return rowtyp
end
row = memoize(row)
Exports.row = row

-------------------------------------------------------------------------------
-- Record Types

local function record_entry(f_num, name, typ)
  if not is_id_str(name) then
    error('expected valid id string for name of field #'..f_num, 4) end
  if not is_type(typ) or not typ:is_value() then
    error('expected value type for field #'..f_num, 4) end
  return { name=name, type=typ }
end
record_entry = memoize_from(2, record_entry)

local reccount = 0
local function record_obj(flist)
  local rtype             = NewType('record')
  rtype.fields            = flist
  reccount = reccount + 1

  local name = newlist{'{ '}
  for i,p in ipairs(flist) do
    if i ~= 1 then name:insert(', ') end
    name:insert(p.name)
    name:insert(':')
    name:insert(tostring(p.type))
  end
  name:insert(' }')
  rtype._name             = name:concat()

  local ttype             = terralib.types.newstruct('rec'..reccount)
  for i,p in ipairs(rtype.fields) do
    ttype.entries:insert { p.name, p.type:terratype() }
  end
  rtype._terra_type       = ttype

  return rtype
end
record_obj = memoize(record_obj)

local function record(fields)
  if not israwlist(fields) then error('expected list of fields as arg', 2) end

  local f = newlist()
  for i,p in ipairs(fields) do
    local name = '_'..(i-1)
    if israwlist(p) and type(p[1]) == 'string' then
      name = p[1]
      p = p[2]
    end
    -- Now, p should be a type, and we'll lift it
    local typ = lift_type_from_lua(p, 2)
    f:insert(record_entry(i, name, typ))
  end

  return record_obj( memolist(f) )
end
Exports.record = record

-------------------------------------------------------------------------------
-- Tensor Types

local function valid_dims(dims, errdepth)
  errdepth = errdepth + 1
  if not israwlist(dims) then
    error('expected tensor dimensions list', errdepth) end
  for i,d in ipairs(dims) do if not is_pos_int(d) then
    error('expected positive integer size for tensor dimension '..i, errdepth)
  end end
end

local function tensor_helper(errdepth, basetyp, dims)
  errdepth = (errdepth or 2)+1

  if basetyp:is_tensor() then
    local dd  = basetyp.dims:copy()
    dd:insertall(dims)
    basetyp   = basetyp._base_type
  end
  assert(is_type(basetyp))

  local tensor        = NewType('tensor')
  tensor.dims         = dims
  tensor._base_type   = basetyp

  local name          = newlist{'tensor('}
                              name:insert(tostring(basetyp))
  for _,d in ipairs(dims) do  name:insert(',')
                              name:insert(tostring(d)) end
                              name:insert(')')
  name                = name:concat()
  tensor._name        = name

  local row_strides   = newlist() -- row major strides
  row_strides[1]      = 1
  for i=2,#dims do
    row_strides[i]    = row_strides[i-1] * dims[i-1]
  end
  tensor.rowstrides   = row_strides
  tensor._n_entries   = row_strides[#dims]*dims[#dims]

  local ttype         = terralib.types.newstruct(name:gsub('[,()]','_'))
  ttype.entries:insert { 'd', basetyp:terratype()[tensor._n_entries] }
  tensor._terra_type  = ttype

  return tensor
end
tensor_helper = memoize_from(2, tensor_helper)

local function tensor(basetyp, ...)
  basetyp = lift_type_from_lua(basetyp, 2)
  if not basetyp:is_value() then error('expected base value type', 2) end
  local dims = {...}
  valid_dims(dims, 2)
  dims = memolist(dims)
  return tensor_helper(2, basetyp, dims)
end
local function matrix(basetyp, N,M)
  basetyp = lift_type_from_lua(basetyp, 2)
  if not basetyp:is_value() then error('expected base value type', 2) end
  local dims = {N,M}
  valid_dims(dims, 2)
  dims = memolist(dims)
  return tensor_helper(2, basetyp, dims)
end
local function vector(basetyp, N)
  basetyp = lift_type_from_lua(basetyp, 2)
  if not basetyp:is_value() then error('expected base value type', 2) end
  local dims = {N}
  valid_dims(dims, 2)
  dims = memolist(dims)
  return tensor_helper(2, basetyp, dims)
end
Exports.tensor    = tensor
Exports.matrix    = matrix
Exports.vector    = vector


-- Internal Type
local tensorindex_nil             = NewType('tensorindex')
      tensorindex_nil._terra_type = nil
      tensorindex_nil._name       = 'tensorindex(?)'
local function tensorindex(n)
  if n == nil then return tensorindex_nil end
  if not is_pos_int(n) then
    error('expected positive integer argument', 2)
  end

  local ti              = NewType('tensorindex')
  ti.range              = n
  ti._name              = 'tensorindex('..tostring(n)..')'
  ti._terra_type        = uint32
  return ti
end
tensorindex = memoize(tensorindex)
Exports.tensorindex = tensorindex

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
--[[              Type Lifting from Lua  &  Tensor Extension               ]]--
-------------------------------------------------------------------------------

-- was forward declared
function lift_type_from_lua(luaval, errdepth)
  errdepth = errdepth + 1
  -- simple case.  Already a type object
  if is_type(luaval) then return luaval end

  -- primitive conversion from Terra type
  if terralib.types.istype(luaval) then
    local ptype = terra_to_primitive_table[luaval]
    if ptype then return ptype
             else error('cannot lift terra type '..tostring(luaval), errdepth)
             end
  end

  -- automatically convert Gong Tables to associated row types
  if S.is_table(luaval) then return row(luaval) end

  -- more complex lifting rules for records; assume all remaining
  -- tables are record types
  if type(luaval) == 'table' then return record(luaval) end

  error('expected a type', errdepth)
end

Type.__index = function(tbl, key)
  if is_pos_int(key) then -- tensor extension
    local n = key
    if tbl._kind == 'tensor' then
      local dims = newlist{unpack(tbl.dims)}; dims:insert(n)
      return tensor(tbl._base_type, unpack(dims))
    elseif tbl._kind == 'primitive' or
           tbl._kind == 'row' or
           tbl._kind == 'record' then
      return vector(tbl,n)
    else
      error('cannot turn type '..tostring(tbl)..' into a vector')
    end
  else
    return rawget(Type, key)
  end
end


-------------------------------------------------------------------------------
--[[                 Type Classification & Access Methods                  ]]--
-------------------------------------------------------------------------------

-- is_type is defined above

-- classification by kind
function Type:is_primitive()    return self._kind == 'primitive'    end
function Type:is_row()          return self._kind == 'row'          end
function Type:is_record()       return self._kind == 'record'       end
function Type:is_tensor()       return self._kind == 'tensor'       end
function Type:is_tensorindex()  return self._kind == 'tensorindex'  end

function Type:is_error()        return self._kind == 'error'        end
function Type:is_internal()     return self._kind == 'internal'     end

function Type:is_value()
  return  self:is_primitive() or
          self:is_row() or
          self:is_record() or
          self:is_tensor()
end

function Type:has_rows()
  if self._has_rows ~= nil then return self._has_rows end

  if      self:is_primitive() then
    self._has_rows = false
  elseif  self:is_row() then
    self._has_rows = true
  elseif  self:is_tensor() then
    self._has_rows = self:basetype():has_rows()
  elseif  self:is_record() then
    local hrs = false
    for _,f in ipairs(self.fields) do
      if f.type:has_rows() then hrs = true; break end
    end
    self._has_rows = hrs
  else
    self._has_rows = false
  end

  return self._has_rows
end

function Type:is_pure_tensor()
  return self:is_tensor() and self:basetype():is_primitive()
end

-------------------------------------------------------------------------------

function Type:is_integral()
  return (self:is_pure_tensor() and self:basetype():is_integral())
      or (self:is_primitive() and self ~= Exports.bool
                              and self ~= Exports.float
                              and self ~= Exports.double)
end

function Type:is_float()
  return (self:is_pure_tensor() and self:basetype():is_float())
      or (self:is_primitive() and (self == Exports.float or
                                   self == Exports.double) )
end

function Type:is_numeric()
  return (self:is_pure_tensor() and self:basetype():is_numeric())
      or (self:is_primitive() and self ~= Exports.bool)
end

function Type:is_logical()
  return (self:is_pure_tensor() and self:basetype():is_logical())
      or self == Exports.bool
end

function Type:is_unknown_tensorindex()
  return self:is_tensorindex() and self.range == nil
end

-------------------------------------------------------------------------------

function Type:basetype()
  if self:is_tensor() then return self._base_type
  else error(':basetype() expected tensor; got '..tostring(self), 2) end
end

local struct emptyTerraType {}
function Type:terratype()
  if self._terra_type then return self._terra_type
  elseif self:is_error() or self:is_internal() then return emptyTerraType
  else error(':terratype() unimplemented for '..tostring(self), 2) end
end

function Type:terrabasetype()
  return self:basetype():terratype()
end

-------------------------------------------------------------------------------

function Type:__tostring()    return self._name     end

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
--[[                         Subtyping & Coercion                          ]]--
-------------------------------------------------------------------------------

--  List of all primitives
--    int8   uint8    size8 
--    int16  uint16   size16
--    int32  uint32   size32
--    int64  uint64   size64
--    bool   float    double
--  Here is a skeleton of the implicit primitive type coercion order
--  The partial order is the transitive closure of these relations
--    size**  <  uint**
--    uint8   <  int16
--    uint16  <  int32
--    uint32  <  int64
--    size8 < size16 < size32 < size64
--    uint8 < uint16 < uint32 < uint64
--     int8 <  int16 <  int32 <  int64
--    float   <  double
--    (uint8,uint16,int8,int16) < float
--        (uint32,int32)        < double

-- NOTE on EXPLICIT primitive coercion
--    In general, we rely on our target language to define
--    the semantics of coercions.  Any attempt at explicit coercion
--    is therefore delegated downward to Terra semantics.
--    They should all be allowed, but with more ambiguous semantics.

-- create an initial reflexive relation
local primitive_coerce    = {}
for _,p in ipairs(primitives) do
  primitive_coerce[p]     = {}
  for _,q in ipairs(primitives) do primitive_coerce[p][q] = T.error end
  primitive_coerce[p][p]  = p
end

-- input the defining relations
primitive_coerce[T.size8 ][T.uint8 ]  = T.uint8
primitive_coerce[T.size16][T.uint16]  = T.uint16
primitive_coerce[T.size32][T.uint32]  = T.uint32
primitive_coerce[T.size64][T.uint64]  = T.uint64

primitive_coerce[T.uint8 ][T.int16 ]  = T.int16
primitive_coerce[T.uint16][T.int32 ]  = T.int32
primitive_coerce[T.uint32][T.int64 ]  = T.int64

  -- promote to larger size within type
primitive_coerce[T.size8 ][T.size16]  = T.size16
primitive_coerce[T.size16][T.size32]  = T.size32
primitive_coerce[T.size32][T.size64]  = T.size64
primitive_coerce[T.uint8 ][T.uint16]  = T.uint16
primitive_coerce[T.uint16][T.uint32]  = T.uint32
primitive_coerce[T.uint32][T.uint64]  = T.uint64
primitive_coerce[T.int8  ][T.int16 ]  = T.int16
primitive_coerce[T.int16 ][T.int32 ]  = T.int32
primitive_coerce[T.int32 ][T.int64 ]  = T.int64
primitive_coerce[T.float ][T.double]  = T.double

primitive_coerce[T.uint16][T.float ]  = T.float
primitive_coerce[T.int16 ][T.float ]  = T.float

primitive_coerce[T.int32 ][T.double]  = T.double
primitive_coerce[T.uint32][T.double]  = T.double

-- take the transitive closure
for i,__ in ipairs(primitives) do -- iterate enough times for convergence
  for p0,sub0 in pairs(primitive_coerce) do
    for p1,val in pairs(sub0) do
      if p0 ~= p1 and val ~= T.error then
        for p2,vv in pairs(primitive_coerce[p1]) do
          if vv == p2 then
            primitive_coerce[p0][p2] = p2
          end
        end
      end
    end
  end
end

--print('\n--\n  Primitives\n--')
--for _,a in ipairs(primitives) do print(a) end
--print('\n--\n  Prim Coerce\n--')
--for _,a in ipairs(primitives) do
--  local line = ' '
--  for _,b in ipairs(primitives) do
--    local p = primitive_coerce[a][b]
--    if p == b then line = line..' X'
--              else assert(p == T.error); line = line..' .' end
--  end
--  print(line)
--end

-------------------------------------------------------------------------------
-- Primitive Coercion Semi-Lattice Join

-- We build the join table of the partial order in the following way
-- Given types A and B, their join is the least type C such that
-- A <= C and B <= C .
-- We can find this type through exhaustive search.
-- If we can't find any such type, then ERROR

local primitive_join    = {}
for _,A in ipairs(primitives) do
  primitive_join[A]     = {}
  for _,B in ipairs(primitives) do
    local mintyp = nil
    for _,C in ipairs(primitives) do
      if primitive_coerce[A][C] == C and
         primitive_coerce[B][C] == C and
         ( mintyp == nil or primitive_coerce[C][mintyp] == mintyp )
      then
        mintyp = C
      end
    end
    primitive_join[A][B] = mintyp or T.error
end end

-------------------------------------------------------------------------------
-- Non-Primitive Coercion

function Type:dims_match(rhs)
  local lhs       = self
  if lhs:is_primitive() and rhs:is_primitive() then return true end
  local ltensor   = lhs:is_pure_tensor()
  local rtensor   = rhs:is_pure_tensor()
  if not ltensor or not rtensor then return false end
  if #lhs.dims ~= #rhs.dims then return false end
  for i=1,#lhs.dims do
    if lhs.dims[i] ~= rhs.dims[i] then return false end
  end
  return true
end

-- can we implicitly coerce the type
function Type:is_coercable_to(target)
  local source = self
  if not is_type(target) then return false end
  -- short circuit for equality
  if source == target then return true end

  -- extract base type if we have tensors
  if source:is_tensor() and target:is_tensor() then
    if not source:dims_match(target) then return false end
    source  = source:basetype()
    target  = target:basetype()
  end

  if source:is_primitive() and target:is_primitive() then
    return primitive_coerce[source][target] ~= T.error
  end

  -- catch-all: fail
  return false
end

-- returns error if the coercion is not valid
function Type:coerce_to(target)
  if self:is_coercable_to(target) then return target
                                  else return T.error end
end

function Type:join(rhs)
  local lhs = self
  if not is_type(rhs) then return T.error end

  -- quick exit on equality
  if lhs == rhs then return lhs end

  -- tensors
  if lhs:is_pure_tensor() and
     rhs:is_pure_tensor() and
     lhs:dims_match(rhs)
  then
    local basejoin = lhs:basetype():join(rhs:basetype())
    return tensor(basejoin, unpack(lhs.dims))
  end

  if lhs:is_primitive() and rhs:is_primitive() then
    return primitive_join[lhs][rhs]
  end

  -- catch-all: fail
  return T.error
end


-------------------------------------------------------------------------------
--[[                       Terra <-> Lua Conversions                       ]]--
-------------------------------------------------------------------------------

local function terra_to_luaval(tval, typ)
  if typ:is_primitive() then
    if typ:is_numeric() then
      return tonumber(tval)
    elseif typ:is_logical() then -- can be tricky
      if type(tval) == 'cdata' then
        return not (tval == 0)
      else
        return tval
      end
    end

  elseif typ:is_tensor() then
    local bt        = typ:basetype()
    local dims      = typ.dims
    local strides   = typ.rowstrides
    -- recursive tensor coversion
    local function tensorconvert(d, offset)
      -- base
      if d > #dims then
        return terra_to_luaval(tval.d[offset], bt)
      -- recurse
      else
        local arr = {}
        for k=1,dims[d] do
          arr[k]  = tensorconvert(d+1, offset)
          offset = offset + strides[d]
        end
        return arr
      end
    end
    return tensorconvert(1,0)

  elseif typ:is_record() then
    local obj = {}
    for _,p in ipairs(typ.fields) do
      obj[p.name] = terra_to_luaval(tval[p.name], p.type)
    end
    return obj

  end

  error("INTERNAL: Should not be trying to convert values of type "..
        tostring(typ).." from Terra to Lua")
end

local function check_luaval(lval, typ)
  if typ:is_primitive() then
    return (typ:is_numeric() and type(lval) == 'number') or
           (typ:is_logical() and type(lval) == 'boolean') 

  elseif typ:is_tensor() then
    local bt    = typ:basetype()
    local dims  = typ.dims
    local function checktensor(v, d)
      if d > #dims then
        return check_luaval(v, bt)
      else
        if type(v) ~= 'table' or #v ~= dims[d] then return false end
        for k=1,dims[d] do
          if not checktensor(v[k], d+1) then return false end
        end
        return true
      end
    end
    return checktensor(lval,1)

  elseif typ:is_record() then
    if type(v) ~= 'table' then return false end
    for _,p in ipairs(typ.fields) do
      if not check_luaval(v[p.name], p.type) then return false end
    end
    return true

  end

  return false
end

local function lua_to_terraval(lval, typ)
  if typ:is_primitive() then
    return lval -- luajit/terra will handle correctly

  elseif typ:is_tensor() then
    local tval    = terralib.new(typ:terratype())

    local bt      = typ:basetype()
    local dims    = typ.dims
    local strides = typ.rowstrides
    local function tensorconvert(v, d, offset)
      if d > #dims then
        tval.d[offset] = lua_to_terraval(v, bt)
      else
        for k=1,dims[d] do
          tensorconvert(v[k], d+1, offset)
          offset = offset + strides[d]
        end
      end
    end
    tensorconvert(lval,1,0)
    return tval

  elseif typ:is_record() then
    local tval = terralib.new(typ:terratype())

    for _,p in ipairs(typ.fields) do
      tval[p.name]  = lua_to_terraval(lval[p.name], p.type)
    end
    return tval

  end
end

Exports.check_luaval      = check_luaval
Exports.lua_to_terraval   = lua_to_terraval
Exports.terra_to_luaval   = terra_to_luaval


-------------------------------------------------------------------------------
--[[                             More Aliasing                             ]]--
-------------------------------------------------------------------------------

for n=2,4 do
  -- common vector aliases
  local vecname = 'vec'..tostring(n)
  T[vecname..'f'] = T.float[n]
  T[vecname..'d'] = T.double[n]
  T[vecname..'i'] = T.int32[n]
  T[vecname..'u'] = T.uint32[n]
  T[vecname..'b'] = T.bool[n]

  -- matrix aliases
  for m=2,4 do
    local matname = 'mat'..tostring(n)..'x'..tostring(m)
    T[matname..'f'] = T.float[n][m]
    T[matname..'d'] = T.double[n][m]
    T[matname..'i'] = T.int32[n][m]
    T[matname..'u'] = T.uint32[n][m]
    T[matname..'b'] = T.bool[n][m]
  end

  -- square matrix aliases
  local shortname   = 'mat'..tostring(n)
  local fullname    = 'mat'..tostring(n)..'x'..tostring(n)
  T[shortname..'f'] = T[fullname..'f']
  T[shortname..'d'] = T[fullname..'d']
  T[shortname..'i'] = T[fullname..'i']
  T[shortname..'u'] = T[fullname..'u']
  T[shortname..'b'] = T[fullname..'b']
end

