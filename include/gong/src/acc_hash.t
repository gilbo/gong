
local Exports = {}
package.loaded["gong.src.acc_hash"] = Exports

-------------------------------------------------------------------------------

local T             = require 'gong.src.types'
local is_type       = T.is_type

local Util          = require 'gong.src.util'
local is_pos_int    = Util.is_pos_int

local StdContainers = require 'gong.src.stdcontainers'
local vector        = StdContainers.vector
local stack         = StdContainers.stack

local C             = require 'gong.src.c'
local assert        = C.assert

local Schemata      = require 'gong.src.schemata'
local is_table      = Schemata.is_table

local Functions     = require 'gong.src.functions'

local AccStructs    = require 'gong.src.acc_structs'

local AccScan         = require 'gong.src.acc_scan'
local new_scan_index  = AccScan.scan_index
local is_scan_index   = AccScan.is_scan_index

local newlist         = terralib.newlist

-------------------------------------------------------------------------------
--[[                           Helper Functions                            ]]--
-------------------------------------------------------------------------------

local fullbitmask = {
  [uint8] = `0xFF,
  [uint16] = `0xFFFF,
  [uint32] = `0xFFFFFFFF,
  [uint64] = `0xFFFFFFFFFFFFFFFFULL,
}

local max = macro(function(a,b) return `terralib.select(a > b, a, b) end)

-------------------------------------------------------------------------------
--[[                    Spatial Index Structure Objects                    ]]--
-------------------------------------------------------------------------------

local Hash_Index      = setmetatable({},AccStructs.SpatialIndex)
Hash_Index.__index    = Hash_Index

function Hash_Index:__newindex(key, val)
  error("Cannot assign members to Hash_Index object",2)
end

function is_hash_index(obj)
  return getmetatable(obj) == Hash_Index
end

local hash_arg_msg = [[
new_hash_index expects named arguments
{
  table       = gong data table to index
  key         = gong type (uint... or uint...[k])
                (int... may be used instead of uint... for keys)
    (at least one of the two abstraction funtions must be supplied)
  abs_range   = gong function : table -> { key, key }
                (the return value is interpreted as
                 lo and hi values of the range)
  abs_point   = gong function : table -> key
  hash        = gong function : key -> uint...
                (if hash is not supplied and the key is a primitive uint type,
                 then the default hash function just returns the key)
  
  BIN_TO_ROW  = ratio of hash-bins to table-rows (optional, default: 2)
}
]]
local function new_hash_index(args)
  if not args.table or
     not args.key or
     not (args.abs_range or args.abs_point)
  then
    error(hash_arg_msg, 2)
  end
  if not is_table(args.table) then
    error("expected 'table' to be a gong data table\n"..hash_arg_msg, 2)
  end
  if not is_type(args.key) or 
     not (  (args.key:is_primitive() and #args.key.dims == 1)
          or args.key:is_pure_tensor() ) or
     not args.key:is_integral()
  then
    error("expected 'key' to be an integer or \n"..
          "vector of integers\n"..hash_arg_msg, 2)
  end

  if args.abs_range and args.abs_point then
    error("expected 'abs_range' or 'abs_point' to be given, but not both\n"..
          hash_arg_msg, 2)
  end
  if args.abs_range then
    if not Functions.is_function(args.abs_range) or
       #args.abs_range:argtypes() ~= 1 or
       args.abs_range:argtypes()[1] ~= T.row(args.table) or
       not args.abs_range:rettype():is_tuple() or
       args.abs_range:rettype() ~= T.record { args.key, args.key }
    then
      error("expected 'abs_range' to be a gong function of type\n"..
            "  table -> { key, key }\n"..hash_arg_msg, 2)
    end
  elseif args.abs_point then
    if not Functions.is_function(args.abs_point) or
       #args.abs_point:argtypes() ~= 1 or
       args.abs_point:argtypes()[1] ~= T.row(args.table) or
       args.abs_point:rettype() ~= args.key
    then
      error("expected 'abs_point' to be a gong function of type\n"..
            "  table -> key\n"..hash_arg_msg, 2)
    end
  end

  if args.hash then
    if not Functions.is_function(args.hash) or
       #args.hash:argtypes() ~= 1 or
       args.hash:argtypes()[1] ~= args.key or
       not args.hash:rettype():is_primitive() or
       not args.hash:rettype():is_integral() or
       args.hash:rettype():is_signed()
    then
      error("expected 'hash' to be a gong function of type\n"..
            "  key -> unsigned_integer_type\n"..hash_arg_msg, 2)
    end

    if #( args.hash:_INTERNAL_geteffects():filter(function(e)
            return e:touches_memory()
          end) ) > 0 then
      error("expected 'hash' to be a pure function", 2) end
  elseif not args.key:is_primitive() then
    error("if 'key' is a vector type, then you must supply\n"..
          "a 'hash' function to collapse the vector key down, of type:\n"..
          "  key -> unsigned_integer_type\n"..hash_arg_msg, 2)
  elseif args.key:is_signed() then
    error("if 'key' is a signed integer, then you must supply\n"..
          "a 'hash' function to convert to an unsigned integer\n"..
          hash_arg_msg, 2)
  end


  local key_base    = (args.key:is_primitive() and args.key)
                                                or args.key:basetype()
  local bin_type    = (args.hash and args.hash:rettype()) or key_base
  local subfs       = newlist()
  if args.abs_range then subfs:insert(args.abs_range) end
  if args.abs_point then subfs:insert(args.abs_point) end
  if args.hash      then subfs:insert(args.hash)      end

  local hashidx = AccStructs.NewSpatialIndexObj({
    _table          = args.table,
    _key            = args.key,
    _key_base       = key_base,
    _bin_type       = bin_type,

    _abs_range      = args.abs_range,
    _abs_point      = args.abs_point,

    _hash           = args.hash,

    _SLOT_BYTES     = 32,
    _BIN_TO_ROW     = args.BIN_TO_ROW or 2,

    _subfuncs       = subfs,
  }, Hash_Index)

  return hashidx
end

function Hash_Index:name() return self._table:name().."_Hash_Index" end



-------------------------------------------------------------------------------
--[[                           Traversal Objects                           ]]--
-------------------------------------------------------------------------------

local Hash_Hash_Traversal       = setmetatable({},AccStructs.Traversal)
Hash_Hash_Traversal.__index     = Hash_Hash_Traversal

function Hash_Hash_Traversal:__newindex(key, val)
  error("Cannot assign members to Hash_Hash_Traversal object",2)
end

local hash_hash_arg_msg = [[
new_hash_hash_traversal() expects named arguments
{
  left        = gong spatial index to traverse
  right       = gong spatial index to traverse
  hash_first  = 'left' or 'right' (default: left)
                ( which table gets hashed first;
                  irrelevant if left == right )
}
]]
local function new_hash_hash_traversal(args)
  if not args.left or
     not args.right
  then
    error(hash_hash_arg_msg, 2)
  end
  if not is_hash_index(args.left) or not is_hash_index(args.right) then
    error("expected 'left' and 'right' to be Hash_Index objects\n"..
          hash_hash_arg_msg, 2)
  end

  if args.left._key ~= args.right._key then
    error("expected 'left' and 'right' to have identical 'key's.", 2)
  end

  local hash_first = args.hash_first
  if hash_first then
    if hash_first ~= 'left' and hash_first ~= 'right' then
      error("expected 'hash_first' to either have value 'left' or 'right'", 2)
    end
  else
    hash_first = 'left'
  end

  local hash_hash_trav = AccStructs.NewTraversalObj({
    _left           = args.left,
    _right          = args.right,
    _hash_first     = hash_first,
    _cpu_enabled    = true,
    _gpu_enabled    = false,
    _subfuncs       = newlist(),
  }, Hash_Hash_Traversal)

  return hash_hash_trav
end

local function is_hash_hash_traversal(obj)
  return getmetatable(obj) == Hash_Hash_Traversal
end


---------------------                   ---------------------


local Scan_Hash_Travesal        = setmetatable({},AccStructs.Traversal)
Scan_Hash_Travesal.__index      = Scan_Hash_Travesal

function Scan_Hash_Travesal:__newindex(key, val)
  error("Cannot assign members to Scan_Hash_Travesal object",2)
end

local scan_hash_arg_msg = [[
new_scan_hash_traversal() expects named arguments
{
  left        = gong hash index to traverse (or table if scan half)
  right       = gong hash index to traverse (or table if scan half)

  (ONLY GIVE ONE OF THE FOLLOWING)
  (whichever side is supplied as a table must have a key abstraction
   function supplied)
  left_abs_range  = gong function : left -> { right.key, right.key }
                    (the return value is interpreted as
                      lo and hi values of the range)
  left_abs_point  = gong function : left -> right.key
  right_abs_range = gong function : right -> { left.key, left.key }
                    (the return value is interpreted as
                      lo and hi values of the range)
  right_abs_point = gong function : right -> left.key
}
]]
local function new_scan_hash_traversal(args)
  local n_key_args  = ((args.left_abs_range and 1) or 0) +
                      ((args.left_abs_point and 1) or 0) +
                      ((args.right_abs_range and 1) or 0) +
                      ((args.right_abs_point and 1) or 0)
  if not args.left or
     not args.right or
     n_key_args ~= 1
  then
    error(scan_hash_arg_msg, 2)
  end
  if not (is_hash_index(args.left) and is_table(args.right)) and
     not (is_table(args.left) and is_hash_index(args.right)) then
    error("expected exactly one of 'left' or 'right' to be a Hash_Index\n"..
          "and the other to be a gong table\n"..
          scan_hash_arg_msg, 2)
  end
  local hash_first  = (is_hash_index(args.left) and 'left') or 'right'
  local hashidx     = (hash_first == 'left' and args.left) or args.right
  local tbl         = (hash_first == 'left' and args.right) or args.left
  local scanidx     = new_scan_index { table = tbl }
  local key         = hashidx._key
  local abs_range   = args.left_abs_range or args.right_abs_range
  local abs_point   = args.left_abs_point or args.right_abs_point

  if abs_range then
    if not Functions.is_function(abs_range) or
       #abs_range:argtypes() ~= 1 or
       abs_range:argtypes()[1] ~= T.row(tbl) or
       not abs_range:rettype():is_tuple() or
       abs_range:rettype() ~= T.record { key, key }
    then
      error("expected 'abs_range' to be a gong function of type\n"..
            "  table -> { key, key }\n"..scan_hash_arg_msg, 2)
    end
  elseif abs_point then
    if not Functions.is_function(abs_point) or
       #abs_point:argtypes() ~= 1 or
       abs_point:argtypes()[1] ~= T.row(tbl) or
       abs_point:rettype() ~= key
    then
      error("expected 'abs_point' to be a gong function of type\n"..
            "  table -> key\n"..scan_hash_arg_msg, 2)
    end
  end

  local scan_hash_trav = AccStructs.NewTraversalObj({
    _left           = (hash_first == 'left' and hashidx) or scanidx,
    _right          = (hash_first == 'right' and hashidx) or scanidx,
    _hash_first     = hash_first,
    _cpu_enabled    = true,
    _gpu_enabled    = false,
    _key            = key,
    _abs_range      = abs_range,
    _abs_point      = abs_point,
    _subfuncs       = newlist{ abs_range or abs_point },
  }, Scan_Hash_Travesal)

  return scan_hash_trav
end

local function is_scan_hash_traversal(obj)
  return getmetatable(obj) == Scan_Hash_Travesal
end


-------------------------------------------------------------------------------
--[[                          Hash Implementation                          ]]--
-------------------------------------------------------------------------------


function Hash_Index:_INTERNAL_StructLayout(StoreAPI)
  local CACHE         = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.HASH then return CACHE.HASH end

  local row_t         = T.row(self._table):terratype()
  local key_t         = self._key:terratype()
  local key_bt        = self._key_base:terratype()
  local bin_t         = self._bin_type:terratype()

  assert(not self._table._is_live, 'INTERNAL: not supporting creating '..
         'a spatial index on a mergable table')
  assert(row_t == uint32, 'INTERNAL: expect table rows to be size uint32')

  local NULL_ROW      = fullbitmask[row_t]
  local NULL_BIN      = fullbitmask[bin_t]
  local NULL_U32      = fullbitmask[uint32]

  local SLOT_IDS      = 1
  do
    local struct slotheader {
      key     : key_t
      nxt     : bin_t
    }
    local sz1         = terralib.sizeof(slotheader)
    local sz_rm       = self._SLOT_BYTES - sz1
    local N           = math.floor( sz_rm/terralib.sizeof(row_t) )
    if N > 1 then SLOT_IDS = N end
  end
  local struct HashSlot {
    key     : key_t
    nxt     : bin_t
    ids     : row_t[SLOT_IDS]
  }
  terra HashSlot:has_nxt()
    return self.nxt ~= NULL_BIN
  end
  terra HashSlot:clear()
    self.nxt = NULL_BIN
    escape for k=1,SLOT_IDS do emit quote
      self.ids[k-1] = NULL_ROW
    end end end
  end

  CACHE.SLOT_IDS      = SLOT_IDS
  CACHE.HashSlot      = HashSlot
  CACHE.NULL_ROW      = NULL_ROW
  CACHE.NULL_BIN      = NULL_BIN
  CACHE.NULL_U32      = NULL_U32


  local struct HashTable {
    n_bin_occupied  : bin_t
    bins            : vector(bin_t)
    slots           : vector(HashSlot)
    rebuild         : bool
    refit           : bool
  }
  --terra HashTable.methods.init     :: &HashTable -> {}
  --terra HashTable.methods.destroy  :: &HashTable -> {}
  terra HashTable:init()
    self.n_bin_occupied = 0
    self.bins:init()
    self.slots:init()
    self.rebuild  = true
    self.refit    = true
  end
  terra HashTable:destroy()
    self.bins:destroy()
    self.slots:destroy()
  end

  CACHE.HashTable     = HashTable

  return HashTable
end

function Hash_Index:_INTERNAL_do_invalidate(idxptr, size_invalid,data_invalid)
  return quote
    --if self.rebuild
    idxptr.rebuild  = idxptr.rebuild  or size_invalid
    idxptr.refit    = idxptr.refit    or data_invalid
  end
end

function Hash_Index:_INTERNAL_PreJoinUpdate( StoreAPI,
                                            name, storeptr, idxptr,
                                            gpu_tblptr, gpu_globptr )
  self:_INTERNAL_Construct_Functions(StoreAPI)

  assert(gpu_tblptr == nil, 'INTERNAL: expect Hash_Index on CPU only')

  return quote end
end

function Hash_Index:_INTERNAL_Construct_Functions(StoreAPI)
  local CACHE           = self:_INTERNAL_get_CACHE(StoreAPI)
  if CACHE.FUNCTIONS_BUILT then return end
  CACHE.FUNCTIONS_BUILT = true

  local row_t           = T.row(self._table):terratype()
  local key_t           = self._key:terratype()
  local key_bt          = self._key_base:terratype()
  local bin_t           = self._bin_type:terratype()

  local SLOT_IDS        = CACHE.SLOT_IDS
  local HashSlot        = CACHE.HashSlot
  local NULL_ROW        = CACHE.NULL_ROW
  local NULL_BIN        = CACHE.NULL_BIN
  local NULL_U32        = CACHE.NULL_U32
  local HashTable       = CACHE.HashTable
  local BIN_TO_ROW      = terralib.constant(double, self._BIN_TO_ROW)

  local abs_range, abs_point, hash
  if self._abs_range then
        abs_range       = StoreAPI:GetCPUFunction( self._abs_range )
  end
  if self._abs_point then
        abs_point       = StoreAPI:GetCPUFunction( self._abs_point )
  end
  if self._hash then
        hash            = StoreAPI:GetCPUFunction( self._hash )
  else
        hash            = macro(function(storeptr, key) return key end)
  end
  CACHE.abs_range       = abs_range
  CACHE.abs_point       = abs_point
  CACHE.hash            = hash

  local key_eq, key_loop, key_join, slot_print
  if self._key:is_primitive() then
    key_eq = macro(function(lhs,rhs) return `lhs == rhs end)
    key_loop = function(lo,hi,key,body)
      return quote
        for i=[lo],[hi]+1 do
          [key] = i
          [body]
        end
      end
    end
    slot_print = macro(function( s )
      local keyfmt, idfmt     = '(%5d)', ''
      local allargs, nullargs = newlist{ `s.key, `s.nxt }, newlist{ `s.key }
      for k=1,SLOT_IDS do
        idfmt = idfmt..' %5d'
        allargs:insert(  `s.ids[k-1] )
        nullargs:insert( `s.ids[k-1] )
      end
      return quote
        if s.nxt == NULL_BIN then
          C.printf([keyfmt..'     - ;'..idfmt], [nullargs])
        else
          C.printf([keyfmt..' %5d ;'..idfmt], [allargs])
        end
      end
    end)
    key_join = macro(function(lhs,rhs) return `max(lhs,rhs) end)
    
  else
    local nD = self._key.dims[1]
    key_eq = macro(function(lhs,rhs)
      local expr  = (`true)
      for k=0,nD-1 do
        local e = expr
        expr = (`[e] and lhs.d[k] == rhs.d[k])
      end
      return expr
    end)
    key_loop = function(lo,hi,key,body)
      local ii        = newlist()
      for j=1,nD do   ii:insert( symbol(key_bt, 'i'..(j-1)) )   end
      local nest      = quote
        [key] = [key_t]({ d = array( [ii] ) })
        [body]
      end
      for j=nD-1,0,-1 do
        local i       = ii[j+1]
        local inner   = nest
        nest          = quote
          for [i]=[lo].d[j],[hi].d[j]+1 do [inner] end
        end
      end
      return nest
    end
    slot_print = macro(function( s )
      local keyfmt, idfmt     = '', ''
      local allargs, nullargs = newlist(), newlist()
      for k=1,nD do
        keyfmt  = keyfmt..((k~=1 and ',') or '')..'%5d'
        allargs:insert(  `s.key.d[k-1] )
        nullargs:insert( `s.key.d[k-1] )
      end
      keyfmt = '('..keyfmt..')'
      allargs:insert(`s.nxt)
      for k=1,SLOT_IDS do
        idfmt = idfmt..' %5d'
        allargs:insert(  `s.ids[k-1] )
        nullargs:insert( `s.ids[k-1] )
      end
      return quote
        if s.nxt == NULL_BIN then
          C.printf([keyfmt..'     - ;'..idfmt], [nullargs])
        else
          C.printf([keyfmt..' %5d ;'..idfmt], [allargs])
        end
      end
    end)
    key_join = macro(function(lhs,rhs)
      local mvec  = newlist()
      for k=0,nD-1 do
        mvec:insert( `max(lhs.d[k], rhs.d[k]) )
      end
      return `[key_t]({ d = array( [mvec] ) })
    end)
  end
  CACHE.key_eq          = key_eq
  CACHE.key_loop        = key_loop
  CACHE.key_join        = key_join

  local RowTyp          = T.row(self._table)
  local StorePtr        = &(StoreAPI:StoreTyp())

  terra HashTable:debug_print()
        C.printf('n_bin_occupied : %d\n', self.n_bin_occupied)
    var n_bins          = self.bins:size()
        C.printf('bins (%d)\n', n_bins)
    for k=0,n_bins do
      var s_id = self.bins(k)
      if s_id == NULL_BIN then
        C.printf('  %6d:      -\n', k)
      else
        C.printf('  %6d: %6d\n', k, s_id)
      end
    end
    var n_slots         = self.slots:size()
        C.printf('slots (%d)\n', n_slots)
    for k=0,n_slots do
      var s = self.slots(k)
      C.printf('  %6d : ', k)
      slot_print(s)
      C.printf('\n')
    end
  end

  terra HashTable:clear( s : StorePtr )
    self.n_bin_occupied = 0
    self.slots:resize(0)

    var n_bins          = [uint32]( C.ceil( [ StoreAPI:Size( s, RowTyp ) ] *
                                            BIN_TO_ROW ) )
    self.bins:resize(n_bins)
    C.memset(self.bins:ptr(), 0xFF, n_bins*sizeof(bin_t))
  end
  terra HashTable:build( s : StorePtr )
    var this    = self
    this:clear(s)
    var row     : row_t
    var key     : key_t
    var bin     : bin_t
    var N_BIN   = [bin_t](self.bins:size())
    escape
      local new_slot    = quote
        var slot                = this.slots:size()
        this.slots:resize(slot+1)
        this.slots(slot):clear()
        this.slots(slot).key    = key
        this.slots(slot).ids[0] = row
      in
        slot
      end
      local add_row     = macro(function(slot) return quote
        var found = false
        for i=0,SLOT_IDS do
          if this.slots(slot).ids[i] == NULL_ROW then
            this.slots(slot).ids[i] = row
            found = true
            break
          end
        end
      in
        found
      end end)
      local insert_code = quote
        var prev_slot   = NULL_BIN
        var slot        = this.bins(bin)
        if slot == NULL_BIN then
          this.n_bin_occupied = this.n_bin_occupied + 1
        end
        -- keep going until either we reach the end of the
        -- linked list of slots at this bin, or we find a spot in
        -- one of those slots to stick this row
        while slot ~= NULL_BIN do
          -- try to add the row to the current slot
          if key_eq( this.slots(slot).key, key ) then
            if add_row(slot) then break end
          end
          -- if we got here, then we could not add this row to this slot,
          -- so advance slots instead...
          prev_slot     = slot
          slot          = this.slots(prev_slot).nxt
        end
        -- we could have exited the loop one of two ways
        -- First, we could have reached the end of the list, in which
        -- case we need to create a new slot (see following)
        -- Second, we could have successfully added the row to
        -- an existing slot, in which case, skip the following
        if slot == NULL_BIN then
          var new_s     = [new_slot]
          if prev_slot == NULL_BIN then
            this.bins(bin)              = new_s
          else
            this.slots(prev_slot).nxt   = new_s
          end
        end
      end
      if abs_range then
        emit(StoreAPI:Scan( s, RowTyp, row, quote
          var lohi      = abs_range(s, row)
          --C.printf('scan %5d:  %5d %5d %5d ; %5d %5d %5d\n', row,
          --          lohi._0.d[0], lohi._0.d[1], lohi._0.d[2],
          --          lohi._1.d[0], lohi._1.d[1], lohi._1.d[2])
          [ key_loop( `lohi._0, `lohi._1, key, quote
              bin = hash(s, key) % N_BIN
              --C.printf('             bin    %5d\n', bin)
              [insert_code]
            end ) ]
        end))
      else assert(abs_point, 'INTERNAL')
        emit(StoreAPI:Scan( s, RowTyp, row, quote
          key         = abs_point(s, row)
          bin         = hash(s, key) % N_BIN
          [insert_code]
        end))
      end
    end

    self.refit = false
    self.rebuild = false
  end
  terra HashTable:update( s : StorePtr )
    if self.rebuild or self.refit then
      self:build(s)
      --self:debug_print()
    end
  end

  local function slot_loop( slot, r, body )
    return quote
      for i=0,SLOT_IDS do
        var [r]   = slot.ids[i]
        --C.printf('      Slotloop %d %d\n', i, r)
        if r == NULL_ROW then break end
        [body]
      end
    end
  end

  HashTable.methods.self_scan = function(
    self, key_sym, row0sym, row1sym, bodycode
  )
    -- loop over the slots, using the trick that the left-hand-row
    -- is only ever assigned based on the current slot, no matter how
    -- far a linked list has to be followed
    return quote
      --self:debug_print()
      var n_slot          = self.slots:size()
      for s0_id=0,n_slot do
        var s0            = self.slots(s0_id)
        var r0 : row_t
        var r1 : row_t
        var [key_sym]     = s0.key
        -- first, self-join within the slot
        [ slot_loop( s0, r0, slot_loop( s0, r1, quote
            var [row0sym] = r0
            var [row1sym] = r1
            if r0 <= r1 then
              [bodycode]
            end
          end )) ]
        -- second, follow the linked list, and loop the initial slot
        -- against those
        var s1_id         = s0.nxt
        while s1_id ~= NULL_BIN do
          var s1          = self.slots(s1_id)
          if key_eq( s0.key, s1.key ) then
            [ slot_loop( s0, r0, slot_loop( s1, r1, quote
                var [row0sym] = r0
                var [row1sym] = r1
                if r1 < r0 then
                  row0sym, row1sym = row1sym, row0sym
                end
                [bodycode]
              end)) ]
          end
          -- end-of-loop linked-list iteration
          s1_id           = s1.nxt
        end
      end
    end
  end

  -- Need to be able to query loop for non-self-join
  HashTable.methods.query_loop = function(
    self, storeptr, key, row_sym, bodycode
  )
    -- select the correct bin/slot for this key and then
    -- loop over the contents
    return quote
      var N_BIN           = [bin_t](self.bins:size())
      var bin             = hash(storeptr, key) % N_BIN
      var slot_id         = self.bins(bin)
      --C.printf('Q-Loop(%d,%d,%d): bin %d\n',key.d[0],key.d[1],key.d[2], bin)
      -- linked list to iterate
      while slot_id ~= NULL_BIN do
        var slot          = self.slots(slot_id)
        --C.printf('  - %d (%d,%d,%d)  \n',slot_id,
        --          slot.key.d[0],slot.key.d[1],slot.key.d[2])
        if key_eq( slot.key, key ) then
          [ slot_loop( slot, row_sym, bodycode ) ]
        end
        -- end-of-loop linked-list iteration
        slot_id           = slot.nxt
      end
    end
  end

end


function Hash_Hash_Traversal:_INTERNAL_PreJoinUpdate(
  L_API, R_API, name, storeptr, idxptr0, idxptr1, gpu_tblptr, gpu_globptr
)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  assert(not gpu_tblptr, 'INTERNAL: expect BVH to be CPU only')
  local HASH_LEFT             = self._hash_first == 'left'
  if HASH_LEFT then
    return `[idxptr0]:update( [storeptr] )
  else
    return `[idxptr1]:update( [storeptr] )
  end
end

function Hash_Hash_Traversal:_INTERNAL_Split_LoopGen(L_API, R_API, for_gpu)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  local CACHE               = self:_INTERNAL_get_CACHE(L_API)
  assert(not for_gpu, 'INTERNAL: expect HashTable to be CPU only')
  return CACHE.Hash_Hash_loopgen
end

function Hash_Hash_Traversal:_INTERNAL_LoopGen(StoreAPI, for_gpu)
  return self:_INTERNAL_Split_LoopGen(StoreAPI, StoreAPI, for_gpu)
end

function Hash_Hash_Traversal:_INTERNAL_Construct_Functions(L_API, R_API)
  local CACHE                 = self:_INTERNAL_get_CACHE(L_API)
  if CACHE.FUNCTIONS_BUILT then return end
  CACHE.FUNCTIONS_BUILT = true

  self._left:_INTERNAL_Construct_Functions(L_API)
  self._right:_INTERNAL_Construct_Functions(R_API)
  local L_CACHE               = self._left:_INTERNAL_get_CACHE(L_API)
  local R_CACHE               = self._right:_INTERNAL_get_CACHE(R_API)

  local HASH_LEFT             = self._hash_first == 'left'
  local IS_SAME_IDX           = (self._left == self._right)

  local Hash_L                = L_CACHE.HashTable
  local Hash_R                = R_CACHE.HashTable
  local RowTyp_L              = T.row(self._left._table)
  local RowTyp_R              = T.row(self._right._table)

  local abs_range_L           = L_CACHE.abs_range
  local abs_range_R           = R_CACHE.abs_range
  local abs_point_L           = L_CACHE.abs_point
  local abs_point_R           = R_CACHE.abs_point
  local hash                  = (HASH_LEFT and L_CACHE.hash) or R_CACHE.hash

  assert( self._left._key == self._right._key, 'INTERNAL' )
  local key_t                 = self._left._key:terratype()
  local key_eq                = L_CACHE.key_eq
  local key_loop              = L_CACHE.key_loop
  local key_join              = L_CACHE.key_join

  local function Hash_Hash_loopgen(storeptr, is_self_join, idxptr0, idxptr1,
                                             row0sym, row1sym, args, bodycode)
    assert(terralib.issymbol(storeptr), 'INTERNAL: expect symbol')
    local StorePtrType  = storeptr.type

    local key           = symbol(key_t, 'key')
    if is_self_join and not IS_SAME_IDX then
      local tmpbody     = bodycode
      bodycode = quote
        if row0sym <= row1sym then
          [tmpbody]
        end
      end
    end
    local deduped_body  = bodycode
    -- only need to deduplicate if we are joining objects where
    -- both sides have been hashed into multiple bins
    if abs_range_L and abs_range_R then
      deduped_body = quote
        var lohi0       = abs_range_L( storeptr, row0sym )
        var lohi1       = abs_range_R( storeptr, row1sym )
        var unq_key     = key_join( lohi0._0, lohi1._0 )
        if key_eq( unq_key, key ) then
          [bodycode]
        end
      end
    end

    local join_loop     = nil
    if IS_SAME_IDX then
      join_loop = Hash_L.methods.self_scan( idxptr0, key,
                                            row0sym, row1sym, deduped_body )
    elseif HASH_LEFT then
      -- SCAN RIGHT ; LOOKUP IN LEFT
      if abs_point_R then
        join_loop = R_API:Scan( storeptr, RowTyp_R, row1sym, quote
            var [key]     = abs_point_R(storeptr, row1sym)
            [ Hash_L.methods.query_loop( idxptr0, storeptr,
                                         key, row0sym, deduped_body ) ]
          end)
      else assert(abs_range_R, 'INTERNAL')
        join_loop = R_API:Scan( storeptr, RowTyp_R, row1sym, quote
            var [key]
            var lo, hi    = abs_range_R(storeptr, row1sym)
            [ key_loop( lo, hi, key,
                Hash_L.methods.query_loop( idxptr0, storeptr,
                                           key, row0sym, deduped_body ) ) ]
          end)
      end
    else -- hash right
      -- SCAN LEFT ; LOOKUP IN RIGHT
      if abs_point_L then
        join_loop = L_API:Scan( storeptr, RowTyp_L, row0sym, quote
            var [key]     = abs_point_L(storeptr, row0sym)
            [ Hash_R.methods.query_loop( idxptr1, storeptr,
                                         key, row1sym, deduped_body ) ]
          end)
      else assert(abs_range_L, 'INTERNAL')
        join_loop = L_API:Scan( storeptr, RowTyp_L, row0sym, quote
            var [key]
            var lo, hi    = abs_range_L(storeptr, row0sym)
            [ key_loop( lo, hi, key,
                Hash_R.methods.query_loop( idxptr1, storeptr,
                                           key, row1sym, deduped_body ) ) ]
          end)
      end
    end

    return join_loop
  end
  CACHE.Hash_Hash_loopgen = Hash_Hash_loopgen
end


function Scan_Hash_Travesal:_INTERNAL_PreJoinUpdate(
  L_API, R_API, name, storeptr, idxptr0, idxptr1, gpu_tblptr, gpu_globptr
)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  assert(not gpu_tblptr, 'INTERNAL: expect BVH to be CPU only')
  local HASH_LEFT             = self._hash_first == 'left'
  if HASH_LEFT then
    return `[idxptr0]:update( [storeptr] )
  else
    return `[idxptr1]:update( [storeptr] )
  end
end

function Scan_Hash_Travesal:_INTERNAL_Split_LoopGen(L_API, R_API, for_gpu)
  self:_INTERNAL_Construct_Functions(L_API, R_API)
  local CACHE               = self:_INTERNAL_get_CACHE(L_API)
  assert(not for_gpu, 'INTERNAL: expect HashTable to be CPU only')
  return CACHE.Scan_Hash_loopgen
end

function Scan_Hash_Travesal:_INTERNAL_LoopGen(StoreAPI, for_gpu)
  return self:_INTERNAL_Split_LoopGen(StoreAPI, StoreAPI, for_gpu)
end

function Scan_Hash_Travesal:_INTERNAL_Construct_Functions(L_API, R_API)
  local CACHE                 = self:_INTERNAL_get_CACHE(L_API)
  if CACHE.FUNCTIONS_BUILT then return end
  CACHE.FUNCTIONS_BUILT = true

  self._left:_INTERNAL_Construct_Functions(L_API)
  self._right:_INTERNAL_Construct_Functions(R_API)
  local L_CACHE               = self._left:_INTERNAL_get_CACHE(L_API)
  local R_CACHE               = self._right:_INTERNAL_get_CACHE(R_API)

  local HASH_LEFT             = self._hash_first == 'left'
  local IS_SAME_IDX           = false
  local hashidx               = (HASH_LEFT and self._left) or self._right
  local hashidxCACHE          = (HASH_LEFT and L_CACHE) or R_CACHE

  local Hash_L                = HASH_LEFT and L_CACHE.HashTable
  local Hash_R                = not HASH_LEFT and R_CACHE.HashTable
  local RowTyp_L              = T.row(self._left._table)
  local RowTyp_R              = T.row(self._right._table)

  local abs_range             = self._abs_range and
                                L_API:GetCPUFunction( self._abs_range )
  local abs_point             = self._abs_point and
                                L_API:GetCPUFunction( self._abs_point )
  local abs_range_L           = abs_range
  local abs_range_R           = abs_range
  local abs_point_L           = abs_point
  local abs_point_R           = abs_point
  if HASH_LEFT then
    abs_range_L               = L_CACHE.abs_range
    abs_point_L               = L_CACHE.abs_point
  else
    abs_range_R               = R_CACHE.abs_range
    abs_point_R               = R_CACHE.abs_point
  end
  local hash                  = (HASH_LEFT and L_CACHE.hash)
                                            or R_CACHE.hash

  assert( self._key == hashidx._key, 'INTERNAL' )
  local key_t                 = self._key:terratype()
  local key_eq                = hashidxCACHE.key_eq
  local key_loop              = hashidxCACHE.key_loop
  local key_join              = hashidxCACHE.key_join

  local function Scan_Hash_loopgen(storeptr, is_self_join, idxptr0, idxptr1,
                                             row0sym, row1sym, args, bodycode)
    assert(terralib.issymbol(storeptr), 'INTERNAL: expect symbol')
    local StorePtrType  = storeptr.type

    local key           = symbol(key_t, 'key')
    if is_self_join then
      local tmpbody     = bodycode
      bodycode = quote
        --C.printf('        %d %d\n', row0sym, row1sym)
        if row0sym <= row1sym then
          [tmpbody]
        end
      end
    end
    local deduped_body  = bodycode
    -- only need to deduplicate if we are joining objects where
    -- both sides have been hashed into multiple bins
    if abs_range_L and abs_range_R then
      deduped_body = quote
        var lohi0       = abs_range_L( storeptr, row0sym )
        var lohi1       = abs_range_R( storeptr, row1sym )
        var unq_key     = key_join( lohi0._0, lohi1._0 )
        if key_eq( unq_key, key ) then
          [bodycode]
        end
      end
    end

    local join_loop     = nil
    if HASH_LEFT then
      -- SCAN RIGHT ; LOOKUP IN LEFT
      if abs_point_R then
        join_loop = R_API:Scan( storeptr, RowTyp_R, row1sym, quote
            --C.printf('r %d\n', row1sym)
            var [key]     = abs_point_R(storeptr, row1sym)
            [ Hash_L.methods.query_loop( idxptr0, storeptr,
                                         key, row0sym, deduped_body ) ]
          end)
      else assert(abs_range_R, 'INTERNAL')
        join_loop = R_API:Scan( storeptr, RowTyp_R, row1sym, quote
            var [key]
            var lo, hi    = abs_range_R(storeptr, row1sym)
            [ key_loop( lo, hi, key,
                Hash_L.methods.query_loop( idxptr0, storeptr,
                                           key, row0sym, deduped_body ) ) ]
          end)
      end
    else -- hash right
      -- SCAN LEFT ; LOOKUP IN RIGHT
      if abs_point_L then
        join_loop = L_API:Scan( storeptr, RowTyp_L, row0sym, quote
            var [key]     = abs_point_L(storeptr, row0sym)
            [ Hash_R.methods.query_loop( idxptr1, storeptr,
                                         key, row1sym, deduped_body ) ]
          end)
      else assert(abs_range_L, 'INTERNAL')
        join_loop = L_API:Scan( storeptr, RowTyp_L, row0sym, quote
            var [key]
            var lo, hi    = abs_range_L(storeptr, row0sym)
            [ key_loop( lo, hi, key,
                Hash_R.methods.query_loop( idxptr1, storeptr,
                                           key, row1sym, deduped_body ) ) ]
          end)
      end
    end

    return join_loop
  end
  CACHE.Scan_Hash_loopgen = Scan_Hash_loopgen
end



-------------------------------------------------------------------------------
--[[                                Exports                                ]]--
-------------------------------------------------------------------------------

Exports.is_hash_index           = is_hash_index
Exports.is_hash_hash_traversal  = is_hash_hash_traversal
Exports.is_scan_hash_traversal  = is_scan_hash_traversal

Exports.hash_index              = new_hash_index
Exports.hash_hash_traversal     = new_hash_hash_traversal
Exports.scan_hash_traversal     = new_scan_hash_traversal



