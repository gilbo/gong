local Exports = {}
package.loaded["gong.src.codegen"] = Exports

-------------------------------------------------------------------------------

local Typechecker = require 'gong.src.typechecker'
local T           = require 'gong.src.types'
local Util        = require 'gong.src.util'
local AST         = Typechecker.AST
--local is_type     = T.is_type
--local SrcInfo     = Util.SrcInfo
--local NewSymbol   = Util.NewSymbol
--local is_symbol   = Util.is_symbol
--local is_id_str   = Util.is_id_str
--local is_int      = Util.is_int
local INTERNAL_ERR  = Util.INTERNAL_ERR

local Schemata    = require 'gong.src.schemata'
local Effects     = require 'gong.src.effectcheck'
local Functions   = require 'gong.src.functions'
local is_function = Functions.is_function
local is_builtin  = Functions.is_builtin
local is_emit     = Effects.Effects.Emit.check
local is_merge    = Effects.Effects.Merge.check

local C             = require 'gong.src.c'
local assert        = C.assert
local StdContainers = require 'gong.src.stdcontainers'
local vector        = StdContainers.vector
local sort          = StdContainers.sort

local PARAMS      = (require 'gong.src.params').get_param
local SLOW_VERIFY = PARAMS('SLOW_VERIFY')

local newlist   = terralib.newlist


-------------------------------------------------------------------------------
--[[                                Buffers                                ]]--
-------------------------------------------------------------------------------

local function buffer(key0_Type, key1_Type, for_gpu)
  assert(not for_gpu, 'INTERNAL: gpu buffers currently unsupported')

  local struct Buf_Entry {
    k0    : key0_Type
    k1    : key1_Type
  }

  local struct Buffer {
    v     : vector(Buf_Entry)
  }
  terra Buffer:init()     self.v:init()     end
  terra Buffer:destroy()  self.v:destroy()  end
  terra Buffer:insert( k0 : key0_Type, k1 : key1_Type )
    var k = self.v:size()
    self.v:resize(k+1)
    self.v(k).k0  = k0
    self.v(k).k1  = k1
  end
  Buffer.methods.replay = function(self, name, k0, k1, args, body)
    return quote
      for k=0,self.v:size() do
        var [k0]        = self.v(k).k0
        var [k1]        = self.v(k).k1
        [body]
      end
    end
  end

  return Buffer
end
buffer = Util.memoize(buffer)

local function lookup_buffer(key0_Type, key1_Type, for_gpu)
  assert(not for_gpu, 'INTERNAL: gpu buffers currently unsupported')

  local struct Buf_Entry {
    k0    : key0_Type
    k1    : key1_Type
  }
  Buf_Entry.metamethods.__lt = macro(function(lhs, rhs)
    return `lhs.k0 < rhs.k0 or (lhs.k0 == rhs.k0 and lhs.k1 < rhs.k1)
  end)
  Buf_Entry.metamethods.__eq = macro(function(lhs, rhs)
    return `lhs.k0 == rhs.k0 and lhs.k1 == rhs.k1
  end)
  Buf_Entry.metamethods.__gt = macro(function(lhs, rhs)
    return `rhs < lhs
  end)

  local struct Buffer {
    v       : vector(Buf_Entry)
    _ready  : bool
  }
  terra Buffer:init()     self.v:init(); self._ready = false    end
  terra Buffer:destroy()  self.v:destroy()                      end
  terra Buffer:insert( k0 : key0_Type, k1 : key1_Type )
    var k = self.v:size()
    self.v:resize(k+1)
    self.v(k).k0  = k0
    self.v(k).k1  = k1
  end
  terra Buffer:prepare()
    [sort(Buf_Entry)]( self.v:ptr(), self.v:size() )
    self._ready = true
  end
  terra Buffer:lookup( k0 : key0_Type, k1 : key1_Type ) : bool
    var VAL         = Buf_Entry { k0 = k0, k1 = k1 }
    assert(self._ready, 'tried to lookup in unprepared buffer')
    -- setup range variables
    var N           = [int32]( self.v:size() )
    if N == 0 then return false end
    var lo, hi      = [int32](0), N-1
    var lo_v, hi_v  = self.v(lo), self.v(hi)
    if VAL < lo_v or VAL > hi_v then return false end

    -- now attempt to narrow range in on the value to lookup
    repeat
      var mid       = (lo+hi)/2
      var mid_v     = self.v(mid)
      if      VAL < mid_v   then    hi = mid-1
      elseif  VAL == mid_v  then    return true
                            else    lo = mid+1      end
    until hi < lo
    return false
  end
  terra Buffer:size() return self.v:size() end

  return Buffer
end
lookup_buffer = Util.memoize(lookup_buffer)

-------------------------------------------------------------------------------
--[[                          Context Definition                           ]]--
-------------------------------------------------------------------------------
local Context = {}
Context.__index = Context

local function NewContext(args)
  local ctxt = setmetatable({
    env           = terralib.newenvironment(nil),
    _W            = args.StoreWrap,
    effects       = args.effects,
    _loop_args    = nil,
    _merge_rms    = {},
    _traversal    = args.traversal,
    _buffer_eff   = args.buffer_effects,
    _buffer_idx   = args.buffer_index,
    _verify_idx   = args.verify_index,
    _effect_enable  = true, -- effects on by default
    _on_gpu_flag  = args.for_gpu,
  }, Context)
  return ctxt
end
function Context:localenv()
  return self.env:localenv()
end

function Context:on_GPU()           return self._on_gpu_flag      end
function Context:buffer_effects()   return self._buffer_eff       end
function Context:buffer_index()     return self._buffer_idx       end
function Context:verify_index()     return self._verify_idx       end
function Context:set_effects_on()   self._effect_enable = true    end
function Context:set_effects_off()  self._effect_enable = false   end
function Context:effects_on()       return self._effect_enable    end

function Context:SetLoopArgs(a1,a2)
  self._loop_args = newlist{a1,a2}
end
function Context:GetLoopArgs()
  if self._loop_args then
    return unpack(self._loop_args)
  end
end

function Context:AddMergeRemove(merge_stmt)
  assert(not self._merge_rms[merge_stmt.dst],
         'INTERNAL: expect no duplicates')
  self._merge_rms[merge_stmt.dst] = merge_stmt
end
function Context:GetMergeRemove(tbl)
  return self._merge_rms[tbl]
end

function Context:GetTerraFunction(obj)
  if is_function(obj) then
    return self._W:get_terra_function(obj, self:on_GPU())
  else INTERNAL_ERR('cannot get Terra function for object type') end
end

function Context:effectflag()
  if self._effect_flag_sym then return self._effect_flag_sym end
  local sym               = symbol( bool, 'effect_flag')
  self._effect_flag_sym   = sym
  return sym
end

function Context:StorePtr()
  if self._store_ptr then return self._store_ptr end
  local ptr               = symbol( &(self._W:Store_Struct()),
                                    'store' )
  self._store_ptr         = ptr
  return ptr
end
function Context:GPU_Tables_Ptr()
  if self._gpu_tables_ptr then return self._gpu_tables_ptr end
  if self:on_GPU() then
    local ptr             = symbol( &(self._W:GPU_Tables_Struct()),
                                    'gpu_tables' )
    self._gpu_tables_ptr  = ptr
    return ptr
  else return nil end
end
function Context:GPU_Globals_Ptr()
  if self._gpu_globals_ptr then return self._gpu_globals_ptr end
  if self:on_GPU() then
    local ptr             = symbol( &(self._W:GPU_Globals_Struct()),
                                    'gpu_globals' )
    self._gpu_globals_ptr = ptr
    return ptr
  else return nil end
end
function Context:NewSym(name, type)
  local ttype             = type:terratype()
  local displayname       = (name and tostring(name)) or nil
  local sym               = symbol( ttype, displayname )
  if name then
    self:localenv()[name] = sym
  end
  return sym
end
function Context:GetSym(name)
  return self:localenv()[name]
end

-- run before operations to ensure that data is in valid
-- locations for the specified effects to occur.
function Context:PrepareData( effects )
  return self._W:PrepareData(self:StorePtr(), self:on_GPU(), effects)
end

function Context:AccIndexInvalidation( effects )
  return self._W:AccIndexInvalidation(self:StorePtr(), effects)
end

function Context:AccIndexUpdate(name)
  if self:on_GPU() then
    return self._W:AccIndexUpdate( self._traversal, name, self:StorePtr(),
                                   self:GPU_Tables_Ptr(),
                                   self:GPU_Globals_Ptr() )
  else
    return self._W:AccIndexUpdate( self._traversal, name, self:StorePtr() )
  end
end

function Context:LoopGen(name, is_self_join, rowA, rowB, args, code)
  if self:on_GPU() then
    return self._W:GPU_LoopGen(name, self:StorePtr(), is_self_join,
                  self:GPU_Tables_Ptr(), self:GPU_Globals_Ptr(),
                  self._traversal, rowA, rowB, args, code)
  else
    return self._W:LoopGen(self:StorePtr(), is_self_join,
                           self._traversal, rowA, rowB, args, code)
  end
end

function Context:DefaultDoubleScan(typA, typB, rowA, rowB, args, code)
  assert(not self:on_GPU(), 'INTERNAL: TODO gpu defaultdoublescan')
  return self._W:DefaultDoubleScan( self:StorePtr(),
                                    typA, typB, rowA, rowB, code )
end

function Context:Read(srctype, row, path)
  if self:on_GPU() then
    return self._W:GPU_Read(self:GPU_Tables_Ptr(), srctype, row, path)
  else
    return self._W:Read(self:StorePtr(), srctype, row, path) end
end

function Context:Write(dsttype, row, path, rval)
  if self:on_GPU() then
    return self._W:GPU_Write(self:GPU_Tables_Ptr(),
                             dsttype, row, path, rval)
  else
    return self._W:Write(self:StorePtr(), dsttype, row, path, rval) end
end

function Context:Reduce(dsttype, op, row, path, rval)
  if self:on_GPU() then
    return self._W:GPU_Reduce(self:GPU_Tables_Ptr(),
                              dsttype, op, row, path, rval)
  else
    return self._W:Reduce(self:StorePtr(), dsttype, op, row, path, rval) end
end

function Context:ReadGlobal(glob, path)
  if self:on_GPU() then
    return self._W:GPU_ReadGlobal(self:GPU_Globals_Ptr(), glob, path)
  else
    return self._W:ReadGlobal(self:StorePtr(), glob, path) end
end

function Context:ReduceGlobal(glob, op, path, rval)
  if self:on_GPU() then
    return self._W:GPU_ReduceGlobal(self:GPU_Globals_Ptr(),
                                    glob, op, path, rval)
  else
    return self._W:ReduceGlobal(self:StorePtr(), glob, op, path, rval) end
end

function Context:Insert(dsttype, vals)
  if self:on_GPU() then
    return self._W:GPU_Insert(self:GPU_Tables_Ptr(),
                              dsttype, vals)
  else
    return self._W:Insert(self:StorePtr(), dsttype, vals) end
end

function Context:PostEmit(dsttype)
  if self:on_GPU() then
    return self._W:GPU_PostEmit(self:StorePtr(), dsttype)
  else return quote end end
end

function Context:PostMerge(dsttype, rm_var, rm_body)
  if self:on_GPU() then
    return self._W:GPU_PostMerge(self:StorePtr(),
                                 self:GPU_Tables_Ptr(),
                                 self:GPU_Globals_Ptr(),
                                 dsttype, rm_var, rm_body)
  else
    return self._W:PostMerge(self:StorePtr(), dsttype, rm_var, rm_body)
  end
end

function Context:MergeLookup(dsttype, row, key0, key1, body, else_vals)
  if self:on_GPU() then
    return self._W:GPU_MergeLookup(self:GPU_Tables_Ptr(), dsttype,
                                   row, key0, key1, body, else_vals)
  else
    return self._W:MergeLookup(self:StorePtr(), dsttype,
                               row, key0, key1, body, else_vals)
  end
end

function Context:KeepRow(dsttype, row)
  if self:on_GPU() then
    return self._W:GPU_KeepRow(self:GPU_Tables_Ptr(), dsttype, row)
  else
    return self._W:KeepRow(self:StorePtr(), dsttype, row)
  end
end

function Context:Profile(name, type, gpu_safe)
  if self:on_GPU() and not gpu_safe then return quote end
  else
    return self._W:Profile(self:StorePtr(), name, type)
  end
end

-------------------------------------------------------------------------------
--[[                               Operators                               ]]--
-------------------------------------------------------------------------------

local minf = macro(function(a,b)
  return quote
    var r = [a]
    var t = [b]
  in terralib.select(t < r, t, r) end
end)
local maxf = macro(function(a,b)
  return quote
    var r = [a]
    var t = [b]
  in terralib.select(t > r, t, r) end
end)

-- The following are all scalar operator generation for single-core CPU
local hugeDouble      = math.huge

local INIT_REDOP = macro(function(op, typ, arg)
  local ttype   = typ:astype()
  assert(ttype:isprimitive(), 'INTERNAL: expect primitive type')
  op = op:asvalue()
  assert(type(op) == 'string', 'INTERNAL: expect string operator')

  local fullbitmask = {
    [uint8] = `0xFF,
    [uint16] = `0xFFFF,
    [uint32] = `0xFFFFFFFF,
    [uint64] = `0xFFFFFFFFFFFFFFFFULL,
    [int8] = `0xFF,
    [int16] = `0xFFFF,
    [int32] = `0xFFFFFFFF,
    [int64] = `0xFFFFFFFFFFFFFFFFLL,
  }
  -- + and * will be handled by the type of arg
  if      op == '+'   then  return quote [arg] = 0 end
  elseif  op == '*'   then  return quote [arg] = 1 end
  -- min and max initialization change value depending on the type...
  elseif  op == 'min' or op == 'max' then
    if ttype == double or ttype == float then
      local inf = math.huge
      if op == 'max' then inf = -inf end
      return quote [arg] = [ttype](inf) end
    elseif not typ:issigned() then
      if op == 'max' then return quote [arg] = 0 end
      else -- op == 'min'
        if fullbitmask[ttype] then return quote [arg] = [fullbitmask[ttype]] end
        else INTERNAL_ERR() end
      end
    else
      if op == 'min' then
        if      ttype == int8  then return quote [arg] = 0x80       end
        elseif  ttype == int16 then return quote [arg] = 0x8000     end
        elseif  ttype == int32 then return quote [arg] = 0x80000000 end
        elseif  ttype == int64 then
                          return quote [arg] = 0x8000000000000000LL end
        else INTERNAL_ERR() end
      else -- op == 'max'
        if      ttype == int8  then return quote [arg] = -0x7F       end
        elseif  ttype == int16 then return quote [arg] = -0x7FFF     end
        elseif  ttype == int32 then return quote [arg] = -0x7FFFFFFF end
        elseif  ttype == int64 then
                          return quote [arg] = -0x7FFFFFFFFFFFFFFFLL end
        else INTERNAL_ERR() end
      end
    end
    return quote [arg] = [math.huge]  end
  elseif  op == 'and'  or op == 'or' then
    if typ:is_logical() then
      if op == 'and' then     return quote [arg] = true end
      elseif op == 'or' then  return quote [arg] = false end
      else INTERNAL_ERR() end
    else
      if op == 'and' then 
        if fullbitmask[ttype] then return quote [arg] = [fullbitmask[ttype]] end
        else INTERNAL_ERR() end
      elseif op == 'or' then  return quote [arg] = false end
      else INTERNAL_ERR() end
    end
  else INTERNAL_ERR('unexpected reduction op: '..tostring(op)) end
end)

local REDUCE_OP = macro(function(op, lhs, rhs)
  op = op:asvalue()
  assert(type(op) == 'string', 'INTERNAL: expect string operator')
  if      op == '+'   then  return quote [lhs] = [lhs] + [rhs] end
  elseif  op == '*'   then  return quote [lhs] = [lhs] * [rhs] end
  elseif  op == 'min' then  return quote [lhs] = minf([lhs], [rhs]) end
  elseif  op == 'max' then  return quote [lhs] = maxf([lhs], [rhs]) end
  elseif  op == 'and' then  return quote [lhs] = [lhs] and [rhs] end
  elseif  op == 'or'  then  return quote [lhs] = [lhs] or [rhs] end
  else INTERNAL_ERR('unexpected reduction op: '..tostring(op)) end
end)

local BINARY_OP = macro(function(op, lhs, rhs)
  op = op:asvalue()
  assert(type(op) == 'string', 'INTERNAL: expect string operator')
  if      op == '+'   then  return `[lhs] + [rhs]
  elseif  op == '-'   then  return `[lhs] - [rhs]
  elseif  op == '*'   then  return `[lhs] * [rhs]
  elseif  op == '/'   then  return `[lhs] / [rhs]
  elseif  op == '%'   then  return `[lhs] % [rhs]
  elseif  op == '<'   then  return `[lhs] < [rhs]
  elseif  op == '>'   then  return `[lhs] > [rhs]
  elseif  op == '<='  then  return `[lhs] <= [rhs]
  elseif  op == '>='  then  return `[lhs] >= [rhs]
  elseif  op == '=='  then  return `[lhs] == [rhs]
  elseif  op == '~='  then  return `[lhs] ~= [rhs]
  elseif  op == 'and' then  return `[lhs] and [rhs]
  elseif  op == 'or'  then  return `[lhs] or [rhs]
  else INTERNAL_ERR('unexpected binary op: '..tostring(op)) end
end)

local UNARY_OP = macro(function(op, arg)
  op = op:asvalue()
  assert(type(op) == 'string', 'INTERNAL: expect string operator')
  if      op == '-'   then  return `-[arg]
  elseif  op == 'not' then  return `not [arg]
  else INTERNAL_ERR('unexpected unary op: '..tostring(op)) end
end)

Exports.INIT_REDOP    = INIT_REDOP
Exports.REDUCE_OP     = REDUCE_OP
Exports.BINARY_OP     = BINARY_OP
Exports.UNARY_OP      = UNARY_OP


-------------------------------------------------------------------------------
--[[                              Entry Point                              ]]--
-------------------------------------------------------------------------------

function Exports.codegen(args)
  local name        = assert(args.name,     'INTERNAL: expect name')
  local ast         = assert(args.ast,      'INTERNAL: expect ast')
  local effects     = assert(args.effects,  'INTERNAL: expect effects')
  local StoreWrap   = assert(args.storewrap,'INTERNAL: expect storewrap')
  local for_gpu     = args.for_gpu
                      assert(for_gpu~=nil,  'INTERNAL: expect for_gpu')
  local traversal       = args.traversal
  local buffer_effects  = args.buffer_effects or false
  local buffer_index    = args.buffer_index   or false
  local verify_index    = args.verify_index   or false

  -- add traversal effects to the join context if present
  if traversal then
    effects:insertall(traversal:_INTERNAL_geteffects())
  end
  local ctxt        = NewContext {
                        effects           = effects,
                        StoreWrap         = StoreWrap,
                        traversal         = traversal,
                        buffer_effects    = buffer_effects,
                        buffer_index      = buffer_index,
                        verify_index      = verify_index,
                        for_gpu           = for_gpu,
                      }
  local func        = ast:codegen(name, ctxt)
  func:setname(name)
  return func
end

local function codegen_all(xs, ctxt)
  return xs:map(function(x) return x:codegen(ctxt) end)
end


-------------------------------------------------------------------------------
--[[                            Top-Level Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Function:codegen(name, ctxt)
  local fargs           = codegen_all(self.args, ctxt)
  local body            = self.body:codegen(ctxt)
  local rettyp          = self.rettype:terratype()
  local args            = newlist()
  if ctxt:on_GPU() then
    args:insertall{ ctxt:GPU_Tables_Ptr(), ctxt:GPU_Globals_Ptr() }
  else
    args:insertall{ ctxt:StorePtr() }
  end
  args:insertall(fargs)

  local func = terra( [args] ) : rettyp
    [body]
  end
  --func:disas()
  return func
end

function AST.Join:codegen(name, ctxt)
  assert(#self.args >= 2 and self.args[1].type:is_row()
                         and self.args[2].type:is_row(), 'INTERNAL')
  local fargs           = codegen_all(self.args, ctxt)
  local typA, typB      = self.args[1].type, self.args[2].type
  local rowA, rowB      = fargs[1], fargs[2]
  local storeptr        = ctxt:StorePtr()
  local innerargs       = newlist()
  if ctxt:on_GPU() then
    innerargs:insertall{ ctxt:GPU_Tables_Ptr(), ctxt:GPU_Globals_Ptr() }
  else
    innerargs:insert(storeptr)
  end
  innerargs:insertall(fargs)
  for k=1,#self.args do fargs[k] = fargs[k+2] end
  local outerargs       = newlist{ storeptr }
  outerargs:insertall(fargs)

  ctxt:SetLoopArgs(rowA,rowB)

  local emittbls        = {}
  local mergetbls       = {}
  for i,e in ipairs(ctxt.effects) do
    if    is_merge(e) then mergetbls[e.dst] = true
    elseif is_emit(e) then emittbls[e.dst]  = true end
  end

  -- buffering code
  local buffer_init       = newlist()
  local buffer_destroy    = newlist()

  -- index and effect buffering setup
  local idxbuf            = nil
  local idxbufType        = nil
  if ctxt:buffer_index() then
    idxbufType            = buffer(typA:terratype(), typB:terratype())
    idxbuf                = symbol(&idxbufType, 'index_buffer')
    buffer_init:insert(quote
      var [idxbuf]  = [&idxbufType]( C.malloc(sizeof(idxbufType)) )
      idxbuf:init()
    end)
    buffer_destroy:insert(quote
      idxbuf:destroy()
      C.free(idxbuf)
    end)
  end
  local effbuf            = nil
  local effbufType        = nil
  if ctxt:buffer_effects() then
    effbufType            = buffer(typA:terratype(), typB:terratype())
    effbuf                = symbol(&effbufType, 'effect_buffer')
    buffer_init:insert(quote
      var [effbuf]  = [&effbufType]( C.malloc(sizeof(effbufType)) )
      effbuf:init()
    end)
    buffer_destroy:insert(quote
      effbuf:destroy()
      C.free(effbuf)
    end)
  end
  local verifybuf         = nil
  local verifybufType     = nil
  if ctxt:verify_index() then
    verifybufType         = lookup_buffer(typA:terratype(), typB:terratype())
    verifybuf             = symbol(&verifybufType, 'verify_buffer')
    buffer_init:insert(quote
      var [verifybuf]  = [&verifybufType]( C.malloc(sizeof(verifybufType)) )
      verifybuf:init()
    end)
    buffer_destroy:insert(quote
      verifybuf:destroy()
      C.free(verifybuf)
    end)
  end

  -- add buffers to argument lists
  local buffered_args     = newlist()
  if effbuf then buffered_args:insert(effbuf) end
  if idxbuf then buffered_args:insert(idxbuf) end
  if verifybuf then buffered_args:insert(verifybuf) end
  buffered_args:insertall(outerargs)

  -- looping structures with buffers
  local index_wrap        = nil
  local effect_wrap       = nil
  if idxbuf then
    local is_self_join    = typA == typB
    index_wrap    = function(body)
      return newlist {
        ctxt:Profile(name..'_loop_time', 'timer_start'),
        ctxt:LoopGen(name, is_self_join, rowA, rowB, buffered_args, quote
          idxbuf:insert(rowA,rowB)
        end),
        ctxt:Profile(name..'_loop_time', 'timer_stop'),
        ctxt:Profile(name..'_post_index_loop_time', 'timer_start'),
        idxbufType.methods.replay( idxbuf, name, rowA, rowB,
                                   buffered_args, body ),
        ctxt:Profile(name..'_post_index_loop_time', 'timer_stop'),
      }
    end
  else
    local is_self_join    = typA == typB
    -- usual case looping over the index without index buffering..
    index_wrap    = function(body)
      return newlist {
        ctxt:Profile(name..'_loop_time', 'timer_start', true),
        ctxt:LoopGen(name, is_self_join, rowA, rowB, buffered_args, body),
        ctxt:Profile(name..'_loop_time', 'timer_stop', true),
      }
    end
  end
  if effbuf then
    -- generate a no-effect version of the inner loop
    ctxt:set_effects_off()
    local no_eff_filter   = self.filter:codegen(ctxt)
    local no_eff_doblock  = self.doblock:codegen(ctxt)
    ctxt:set_effects_on()

    local no_eff_innerloop = terra( [innerargs] ) : bool
      var [ctxt:effectflag()] = false
      [no_eff_filter]
      [no_eff_doblock]
      return [ctxt:effectflag()]
    end
    --no_eff_innerloop:disas()

    effect_wrap   = function(loopcall)
      -- first loop(s) to fill up effect buffer
      local idx_stmts = index_wrap(quote
        [ ctxt:Profile(name..'_post_index_rows', 'framed_counter') ]
        var eff_flag    = no_eff_innerloop( [innerargs] )
        if eff_flag then
          [ ctxt:Profile(name..'_effectful_rows', 'framed_counter') ]
          effbuf:insert(rowA, rowB)
        end
      end)
      -- remaining processing of effects
      idx_stmts:insertall {
        ctxt:Profile(name..'_effectful_loop_time', 'timer_start'),
        effbufType.methods.replay( effbuf, name, rowA, rowB,
                                   buffered_args, loopcall ),
        ctxt:Profile(name..'_effectful_loop_time', 'timer_stop'),
      }
      return idx_stmts
    end
  else
    effect_wrap   = function(loopcall)
      return index_wrap(quote
        [ ctxt:Profile(name..'_post_index_rows', 'framed_counter') ]
        var eff_flag    = [loopcall]
        if eff_flag then
          [ ctxt:Profile(name..'_effectful_rows', 'framed_counter') ]
        end
      end)
    end
  end

  -- now, plug in the actual, effectful inner loop, agnostic to whether
  -- any other loops were created to replay buffers from etc.
  local filter          = self.filter:codegen(ctxt)
  local doblock         = self.doblock:codegen(ctxt)

  local innerloop  = terra( [innerargs] ) : bool
    var [ctxt:effectflag()] = false
    [filter]
    [doblock]
    return [ctxt:effectflag()]
  end
  --innerloop:disas()

  local loopcall = (`innerloop( [innerargs] ))
  if verifybuf then
    loopcall = quote
      var eff_flag = innerloop( [innerargs] )
      if eff_flag then
        verifybuf:insert(rowA, rowB)
      end
    in eff_flag end
  end

  local loopstmts = effect_wrap(loopcall)

  -- add additional loop if verifying...
  if verifybuf then
    -- generate a no-effect version of the inner loop
    ctxt:set_effects_off()
    local no_eff_filter   = self.filter:codegen(ctxt)
    local no_eff_doblock  = self.doblock:codegen(ctxt)
    ctxt:set_effects_on()

    local no_eff_innerloop = terra( [innerargs] ) : bool
      var [ctxt:effectflag()] = false
      [no_eff_filter]
      [no_eff_doblock]
      return [ctxt:effectflag()]
    end
    --no_eff_innerloop:disas()

    -- raw loop...
    local n_pairs = symbol(uint64, 'n_pairs')
    loopstmts:insertall {
    quote
      verifybuf:prepare()
      var [n_pairs]       = 0
    end,
    ctxt:DefaultDoubleScan(typA, typB, rowA, rowB, buffered_args,
    quote
      var eff_flag        = no_eff_innerloop( [innerargs] )
      if eff_flag then
        [n_pairs]         = [n_pairs] + 1
        var found_earlier = verifybuf:lookup(rowA, rowB)
        assert(found_earlier, "VERIFICATION FAILED: missing result %d %d",
                              rowA, rowB)
      end
    end),
    quote
      assert([n_pairs] == verifybuf:size(),
             "VERIFICATION FAILED: got %d results, expected %d results",
             verifybuf:size(), [n_pairs])
    end,
    }
  end

  local outerloop = terra( [outerargs] ) escape
    emit( ctxt:PrepareData( ctxt.effects ) )

    -- do index maintenance now if needed
    emit( ctxt:Profile(name..'_index_updates', 'timer_start', true) )
    emit( ctxt:AccIndexUpdate( name ) )
    emit( ctxt:Profile(name..'_index_updates', 'timer_stop', true) )

    -- buffer initialization
    emit quote [buffer_init] end

    -- then do the main loops of the join
    emit quote [loopstmts] end
    -- buffer cleanup
    emit quote [buffer_destroy] end

    emit( ctxt:Profile(name..'_post_index_rows', 'end_framed_counter') )
    emit( ctxt:Profile(name..'_effectful_rows', 'end_framed_counter') )

    -- Invalidate any spatial indices that need to be invalidated,
    -- as a result of this join modifying some data
    emit( ctxt:AccIndexInvalidation( ctxt.effects ) )

    -- cleanup any merge tables
    if next(mergetbls) then
      emit( ctxt:Profile(name..'_merge_cleanup_time', 'timer_start', true) )
      for dst,_ in pairs(mergetbls) do
        local mstmt           = ctxt:GetMergeRemove(dst)
        local rm_var, rm_body = nil, nil
        if mstmt then
          rm_var              = ctxt:NewSym(mstmt.rm_name, mstmt.dst)
          rm_body             = mstmt.rm_body:codegen(ctxt)
          -- hack to prevent undefined variable error
          rm_body = quote
            var [ctxt:effectflag()] = false
            [rm_body]
          end
        end
        emit( ctxt:PostMerge(dst, rm_var, rm_body) )
      end
      emit( ctxt:Profile(name..'_merge_cleanup_time', 'timer_stop', true) )
    end
    if next(emittbls) then
      for dst,_ in pairs(emittbls) do
        emit( ctxt:PostEmit(dst) )
      end
    end
  end end
  return outerloop --, newlist{ innerloop }
end

function AST.ArgDecl:codegen(ctxt)
  return ctxt:NewSym(self.name, self.type)
end


-------------------------------------------------------------------------------
--[[                            Statement Rules                            ]]--
-------------------------------------------------------------------------------

function AST.Block:codegen(ctxt)
  local stmts     = newlist()
  for _,s in ipairs(self.stmts) do
    stmts:insert(   s:codegen(ctxt) )
  end
  return stmts
end

function AST.WhereFilter:codegen(ctxt)
  local expr      = self.expr:codegen(ctxt)
  return quote
    if not [expr] then return [ctxt:effectflag()] end
  end
end
function AST.EmitStmt:codegen(ctxt)
  local exprs     = codegen_all(self.record.exprs, ctxt)
  local dst       = self.dst
  return quote 
    [ ctxt:effectflag() ] = true
    [ (ctxt:effects_on() and ctxt:Insert(dst, exprs)) or {} ]
  end
end
function AST.MergeStmt:codegen(ctxt)
  if not ctxt:effects_on() then
    return quote
      [ ctxt:effectflag() ] = true
    end
  end
  if self.rm_name then
    ctxt:AddMergeRemove(self)
  end
  local up_var    = ctxt:NewSym(self.up_name, self.dst)
  local k0, k1    = ctxt:GetLoopArgs()
  local up_body   = self.up_body:codegen(ctxt)
  local new_vals  = nil
  if self.new_emit then
    new_vals      = codegen_all(self.new_emit.record.exprs, ctxt)
  end
  return quote
    [ ctxt:effectflag() ] = true
    [ ctxt:MergeLookup(self.dst, up_var, k0, k1, up_body, new_vals) ]
  end
end
function AST.KeepStmt:codegen(ctxt)
  local arg       = self.arg:codegen(ctxt)
  return ctxt:KeepRow(self.arg.type, arg)
end
function AST.ReturnStmt:codegen(ctxt)
  local expr      = self.expr:codegen(ctxt)
  return quote
    return [expr]
  end
end
function AST.BreakStmt:codegen(ctxt)
  return quote break end
end
function AST.BuiltInStmt:codegen(ctxt)
  local args      = codegen_all(self.args, ctxt)
  local call      = self.builtin._codegen(args, self, ctxt)
  return call
end


local function range_check(expr, lo, hi, ast, ctxt)
  -- only insert range checks in slow mode
  if not SLOW_VERIFY then return expr end

  if ctxt:on_GPU() then return expr
  else
    return quote
      var e = [expr]
      assert(e >= lo and e < hi,
             [tostring(ast.srcinfo)..':  OUT OF BOUND INDEX!  %d in [%d,%d)'],
             e, lo, hi)
    in e end
  end
end
function AST.PathField:codegen(ctxt)
  return self.name
end
function AST.PathIndex:codegen(ctxt)
  local args      = codegen_all(self.args, ctxt)
  local dims      = self.basetyp.dims
  for k=1,#args do
    args[k]       = range_check(args[k], 0, dims[k], self, ctxt)
  end
  return args
end
function AST.Assignment:codegen(ctxt)
  local rval      = self.rval:codegen(ctxt)
  if AST.TableWrite.check(self.lval) then
    local row     = self.lval.base:codegen(ctxt)
    local path    = codegen_all(self.lval.path, ctxt)
    local dst     = self.lval.base.type
    return quote
      [ ctxt:effectflag() ] = true
      [ (ctxt:effects_on() and ctxt:Write(dst, row, path, rval)) or {} ]
    end
  else
    local lval    = self.lval:codegen(ctxt)
    return quote [lval] = [rval] end
  end
end
function AST.DeclStmt:codegen(ctxt)
  local symname   = ctxt:NewSym(self.name, self.type)
  local rval      = self.rval:codegen(ctxt)
  return quote var [symname] = [rval] end
end
local function reduction_lval_unwind(lval, ctxt)
  if AST.RecordRead.check(lval) then
    local expr, stmts     = reduction_lval_unwind(lval.base, ctxt)
    return (`expr.[lval.arg]), stmts
  elseif AST.TensorIndex.check(lval) then
    local expr, stmts     = reduction_lval_unwind(lval.base, ctxt)
    -- cache computed offset
    local offset          = `0
    local strides         = lval.base.type.rowstrides
    local dims            = lval.base.type.dims
    for i,a in ipairs(lval.args) do
      local aexpr         = a:codegen(ctxt)
      aexpr               = range_check(aexpr, 0, dims[i], lval, ctxt)
      local off           = offset
      offset              = `[off] + [ strides[i] ] * [aexpr]
    end
    local offname         = ctxt:NewSym(nil, lval.args[1].type)
    stmts:insert(quote var [offname] = [offset] end)
    return (`expr.d[ offname ]), stmts
  elseif AST.Var.check(lval) then
    return lval:codegen(ctxt), newlist()
  else INTERNAL_ERR("Unexpected lval") end
end
function AST.Reduction:codegen(ctxt)
  local rval      = self.rval:codegen(ctxt)
  if AST.TableWrite.check(self.lval) then
    local row     = self.lval.base:codegen(ctxt)
    local path    = codegen_all(self.lval.path, ctxt)
    local dst     = self.lval.base.type
    return quote
      [ ctxt:effectflag() ] = true
      [ (ctxt:effects_on() and ctxt:Reduce(dst, self.op, row, path, rval))
                            or {} ]
    end
  elseif AST.GlobalWrite.check(self.lval) then
    local path    = codegen_all(self.lval.path, ctxt)
    local glob    = self.lval.base
    return quote
      [ ctxt:effectflag() ] = true
      [ (ctxt:effects_on() and ctxt:ReduceGlobal(glob, self.op, path, rval))
                            or {} ]
    end
  else
    local lval, stmts   = reduction_lval_unwind(self.lval, ctxt)
    local op            = self.op
    local typ           = self.lval.type
    local rtyp          = self.rval.type
    if typ:is_tensor() then
      local btyp        = typ:basetype()
      local Nd          = typ._n_entries
      return quote
        [stmts]
        var rhs = [rval]
        for k=0,Nd do
          REDUCE_OP(op, lval.d[k], [ (rtyp:is_tensor() and `rhs.d[k])
                                                        or `rhs ])
        end
      end
    else
      return quote
        [stmts]
        REDUCE_OP(op, lval, rval)
      end
    end
  end
end

function AST.IfStmt:codegen(ctxt)
  local stmt          = newlist()
  if self.else_body then
    stmt              = self.else_body:codegen(ctxt)
  end
  for i=#self.cases,1,-1 do
    local cond        = self.cases[i].cond:codegen(ctxt)
    local body        = self.cases[i].body:codegen(ctxt)
    local prev        = stmt
    stmt              = quote if [cond] then [body] else [prev] end end
  end
  return stmt
end
function AST.ForLoop:codegen(ctxt)
  local itername      = ctxt:NewSym(self.itername, self.lo.type)
  local lo            = self.lo:codegen(ctxt)
  local hi            = self.hi:codegen(ctxt)
  local stride        = (self.stride and self.stride:codegen(ctxt)) or nil
  local body          = self.body:codegen(ctxt)
  if stride then
    return quote for [itername] = [lo],[hi],[stride] do [body] end end
  else
    return quote for [itername] = [lo],[hi] do [body] end end
  end
end


-------------------------------------------------------------------------------
--[[                            Expression Rules                           ]]--
-------------------------------------------------------------------------------

function AST.Var:codegen(ctxt)
  local symname       = ctxt:GetSym(self.name)
  return symname
end
function AST.LuaObj:codegen(ctxt)
  local val = self.type:is_internal() and self.type.value
  if val and type(val) == 'string' then
    return val
  else
    INTERNAL_ERR("Should not have LuaObjects left at Codegen")
  end
end
function AST.NumLiteral:codegen(ctxt)
  local ttype         = self.type:terratype()
  return `[ttype]( [self.value] )
end
function AST.BoolLiteral:codegen(ctxt)
  return self.value
end

--------------------------------------

function AST.RecordExpr:codegen(ctxt)
  local exprs         = codegen_all(self.exprs, ctxt)
  local ttype         = self.type:terratype()
  return `[ttype]({ [exprs] })
end
function AST.ListExpr:codegen(ctxt)
  local exprs         = codegen_all(self.exprs, ctxt)
  local ttype         = self.type:terratype()
  local stride        = self.type.rowstrides[#self.type.rowstrides]

  if stride == 1 then
    return `[ttype]({ d=array( [exprs] ) })
  else
    return quote
      var res : ttype
      escape for k,e in ipairs(exprs) do
        local base    = stride * (k-1)
        emit quote
          var tempval = [e] -- bind to prevent recompute
          for i=0,stride do res.d[base+i] = tempval.d[i] end -- copy
        end
      end end
    in res end
  end
end

--------------------------------------

function AST.TensorIndex:codegen(ctxt)
  local strides       = self.base.type.rowstrides
  local base          = self.base:codegen(ctxt)
  local args          = codegen_all(self.args, ctxt)
  local dims          = self.base.type.dims
  local offset        = range_check( args[1], 0, dims[1], self, ctxt )
  for k=2,#args do
    local off         = offset
    local a           = range_check( args[k], 0, dims[k], self, ctxt )
    offset            = `[off] + [ strides[k] ] * [ args[k] ]
  end
  return `[base].d[ offset ]
end
function AST.RecordRead:codegen(ctxt)
  local base          = self.base:codegen(ctxt)
  return `[base].[ self.arg ]
end
function AST.TableRead:codegen(ctxt)
  local row           = self.base:codegen(ctxt)
  local path          = codegen_all(self.path, ctxt)
  local dst           = self.base.type
  return ctxt:Read(dst, row, path)
end
function AST.TableWrite:codegen(ctxt)
  INTERNAL_ERR('Should Never Call Directly')
end
function AST.GlobalRead:codegen(ctxt)
  local path          = codegen_all(self.path, ctxt)
  local glob          = self.base
  return ctxt:ReadGlobal(glob, path)
end
function AST.GlobalWrite:codegen(ctxt)
  INTERNAL_ERR('Should Never Call Directly')
end

--------------------------------------

function AST.BinaryOp:codegen(ctxt)
  local ltyp          = self.lhs.type
  local rtyp          = self.rhs.type
  local typ           = self.type
  local op            = self.op
  local lhs           = self.lhs:codegen(ctxt)
  local rhs           = self.rhs:codegen(ctxt)
  if ltyp:is_row() and rtyp:is_row() then
    return `BINARY_OP(op,lhs,rhs)
  elseif ltyp:is_primitive() and rtyp:is_primitive() then
    return `BINARY_OP(op,lhs,rhs)
  elseif op == '==' or op == '~=' then
    local btyp        = ltyp:basetype()
    local Nd          = typ._n_entries
    local foldop      = macro(function(a,b)
                                if op == '==' then return `a and b
                                              else return `a or b end end)
    return quote
      var res : bool  = [ (op == '==' and true ) or false ]
      var lv          = [lhs]
      var rv          = [rhs]
      for i=0,Nd do
        res = foldop( res, BINARY_OP(op, lv.d[i], rv.d[i]) )
      end
    in res end
  else
    local ttype       = typ:terratype()
    local btyp        = typ:basetype()
    local Nd          = typ._n_entries
    return quote
      var res : ttype
      var lv          = [lhs]
      var rv          = [rhs]
      for i=0,Nd do escape
        local lval    = (ltyp:is_tensor() and `lv.d[i]) or `lv
        local rval    = (rtyp:is_tensor() and `rv.d[i]) or `rv
        emit quote res.d[i] = BINARY_OP(op, lval, rval) end
      end end
    in res end
  end
end
function AST.UnaryOp:codegen(ctxt)
  local typ           = self.type
  local op            = self.op
  local expr          = self.expr:codegen(ctxt)
  if typ:is_primitive() then
    return `UNARY_OP(op,expr)
  else
    local ttype       = typ:terratype()
    local btyp        = typ:basetype()
    local Nd          = typ._n_entries
    return quote
      var res : ttype
      var e = [expr]
      for i=0,Nd do res.d[i] = UNARY_OP(op, e.d[i]) end
    in res end
  end
end

function AST.TernaryExpr:codegen(ctxt)
  local cond          = self.cond:codegen(ctxt)
  local lhs           = self.lhs:codegen(ctxt)
  local rhs           = self.rhs:codegen(ctxt)
  return `terralib.select(cond, lhs, rhs)
end
function AST.LetExpr:codegen(ctxt)
  local stmts         = self.block:codegen(ctxt)
  local expr          = self.expr:codegen(ctxt)
  return quote [stmts] in [expr] end
end

--------------------------------------

function AST.Cast:codegen(ctxt)
  local ttype         = self.type:terratype()
  local arg           = self.arg:codegen(ctxt)
  if self.type:is_tensor() then
    local Nelem       = self.type._n_entries
    local btype       = self.type:terrabasetype()
    return quote
      var res : ttype
      var a   = [arg] -- bind to prevent re-evaluation
      for k=0,Nelem do res.d[k] = [btype]( a.d[k] ) end
    in res end
  else
    return `[ttype](arg)
  end
end
function AST.BuiltInCall:codegen(ctxt)
  local args          = codegen_all(self.args, ctxt)
  local call          = self.builtin._codegen(args, self, ctxt)
  return call
end
function AST.FuncCall:codegen(ctxt)
  local args          = codegen_all(self.args, ctxt)
  local func          = ctxt:GetTerraFunction(self.func)
  if ctxt:on_GPU() then
    local gpuptr      = ctxt:GPU_Tables_Ptr()
    local globptr     = ctxt:GPU_Globals_Ptr()
    return `func( gpuptr, globptr, [args] )
  else
    local storeptr      = ctxt:StorePtr()
    return `func( storeptr, [args] )
  end
end

-- WARNING: Destructive to names and dims lists
local function tensorop_loops( names, dims, body )
  if #names == 0 then
    return body
  else
    local nm            = names:remove()
    local d             = dims:remove()
    local subcall       = tensorop_loops(names, dims, body)
    return quote for [nm]=0,d do [subcall] end end
  end
end
local function unroll_tensorop_loops( names, dims, body )
  if #names == 0 then
    return body
  else
    local nm            = names:remove()
    local d             = dims:remove()
    local subs          = newlist()
    for i=0,d-1 do
      subs:insert(quote do var [nm] = [i-1]; [body] end end)
    end
    body = quote [subs] end
    return unroll_tensorop_loops(names, dims, body)
  end
end
local function unroll_tensorop_list( names, dims, expr )
  if #names == 0 then
    return newlist{ expr }
  else
    local nm            = names:remove()
    local d             = dims:remove()
    local sub           = unroll_tensorop_list(names, dims, expr)
    local out           = newlist()
    for _,e in ipairs(sub) do
      for i=1,d do
        out:insert(quote var [nm] = [i-1] in [e] end)
      end
    end
    return out
  end
end
local function tensorop_prelude(self, ctxt)
  local names         = newlist()
  local mapdims       = newlist()
  for i,nm in ipairs(self.names) do
    names[i]          = ctxt:NewSym(nm, self.idxtypes[i])
    mapdims[i]        = self.idxtypes[i].range
  end
  local expr          = self.expr:codegen(ctxt)
  return names, mapdims, expr
end
function AST.TensorMap:codegen(ctxt)
  local names, mapdims, expr = tensorop_prelude(self, ctxt)

  local ttype         = self.type:terratype()
  local strides       = self.type.rowstrides
  local etype         = self.expr.type
  local ed            = (etype:is_tensor() and etype._n_entries) or 1

  local offset        = `[ ed*strides[1] ] * [ names[1] ]
  for k=2,#names do
    local off         = offset
    offset            = `[off] + [ ed*strides[k] ] * [ names[k] ]
  end

  return quote
    var res : ttype
    escape if etype:is_tensor() then
      emit(tensorop_loops(names, mapdims, quote
        var etmp          = [expr]
        for k=0,ed do
          res.d[offset+k] = etmp.d[k]
        end
      end))
    else
      emit(tensorop_loops(names, mapdims, quote
        res.d[offset]     = [expr]
      end))
      --print("ANFSDF:LNJ")
      --emit(quote C.printf("LOOP B\n") end)
      --emit(unroll_tensorop_loops(names, mapdims, quote
      --  res.d[offset]     = [expr]
      --end))
      --print("A(NUF)(*&HEWPO:LNJ")
      --emit(quote C.printf("  LOOP E\n") end)
    end end
  in res end
end
function AST.TensorFold:codegen(ctxt)
  local names, mapdims, expr = tensorop_prelude(self, ctxt)

  local op            = self.op
  local typ           = self.type
  local btyp          = (typ:is_tensor() and typ:terrabasetype()) or nil
  local ttype         = self.type:terratype()
  local td            = (typ:is_tensor() and typ._n_entries) or 1

  return quote
    var res : ttype
    escape if typ:is_tensor() then
      local es = unroll_tensorop_list(names, mapdims, expr)
      emit quote res = [ es[1] ] end
      for i=2,#es do emit quote
        var etmp = [ es[i] ]
      escape for k=0,td-1 do emit quote
        REDUCE_OP(op, res.d[k], empt.d[k])
      end end end
      end end

      --emit quote
      --for k=0,td do INIT_REDOP(op, btyp, res.d[k]) end
      --[tensorop_loops(names, mapdims, quote
      --  var etmp          = [expr]
      --  for k=0,td do REDUCE_OP(op, res.d[k], etmp.d[k]) end
      --end)] end
    else
      local es = unroll_tensorop_list(names, mapdims, expr)
      emit quote res = [ es[1] ] end
      for i=2,#es do
        emit quote REDUCE_OP(op, res, [ es[i] ]) end
      end

      --emit quote
      --INIT_REDOP(op, ttype, res)
      --[tensorop_loops(names, mapdims, quote
      --  REDUCE_OP(op, res, expr)
      --end)] end
    end end
  in res end
end




