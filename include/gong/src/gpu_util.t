-- The MIT License (MIT)
-- 
-- Copyright (c) 2015 Stanford University.
-- All rights reserved.
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

-- Mostly coppied from Ebb
-- Also borrowing some code from Opt

local Exports                       = {}
package.loaded['gong.src.gpu_util']  = Exports

local PARAMETER     = (require 'gong.src.params').get_param

-- Do NOT execute this file if the gpu is not turned on...
if not PARAMETER('GPU_ENABLED') then return end

------------------------------------------------------
local C             = require 'gong.src.c'
local V             = require 'gong.src.verbosity'

local newlist       = terralib.newlist

local WARPSIZE      = PARAMETER('WARPSIZE')
local MAX_BLOCK_DIM = PARAMETER('GPU_MAX_BLOCK_DIM')

local CUB           = require 'gong.src.cub_wrap'


-------------------------------------------------------------------------------
-- Threads, Grids, Blocks
-------------------------------------------------------------------------------

-- thread ids in each dimension
local tid_x   = cudalib.nvvm_read_ptx_sreg_tid_x
local tid_y   = cudalib.nvvm_read_ptx_sreg_tid_y
local tid_z   = cudalib.nvvm_read_ptx_sreg_tid_z

-- number of threads-per-block in each dimension
local b_dim_x = cudalib.nvvm_read_ptx_sreg_ntid_x
local b_dim_y = cudalib.nvvm_read_ptx_sreg_ntid_y
local b_dim_z = cudalib.nvvm_read_ptx_sreg_ntid_z

-- block ids in each dimension
local bid_x   = cudalib.nvvm_read_ptx_sreg_ctaid_x
local bid_y   = cudalib.nvvm_read_ptx_sreg_ctaid_y
local bid_z   = cudalib.nvvm_read_ptx_sreg_ctaid_z

-- number of blocks in each dimension
local g_dim_x = cudalib.nvvm_read_ptx_sreg_nctaid_x
local g_dim_y = cudalib.nvvm_read_ptx_sreg_nctaid_y
local g_dim_z = cudalib.nvvm_read_ptx_sreg_nctaid_z

local thread_id = macro(function()
  return `(tid_x() +
           tid_y() * b_dim_x() +
           tid_z() * b_dim_x() * b_dim_y())
end)

local block_id = macro(function()
  return `(bid_x() +
           bid_y() * g_dim_x() + 
           bid_z() * g_dim_x() * g_dim_y())
end)

local num_threads_per_block = macro(function()
  return `(b_dim_x() * b_dim_y() * b_dim_z())
end)
local num_blocks = macro(function()
  return `(g_dim_x()*g_dim_y()*g_dim_z())
end)

local global_tid = macro(function()
  return `(thread_id() + block_id()*num_threads_per_block())
end)

-- this routine will convert a linear number of
-- blocks into a 
local terra get_grid_dimensions(
  num_blocks : uint64
) : {uint32, uint32, uint32}
  if num_blocks < MAX_BLOCK_DIM then
    return { num_blocks, 1, 1 }
  else
    -- num_blocks should never be large enough to trip this assert
    -- probably something else would break first...
    C.assert(num_blocks / MAX_BLOCK_DIM < MAX_BLOCK_DIM,
             'INTERNAL: way too many threads on GPU launch')
    -- fit the blocks into the squarest packing we can
    var N = [uint32] (C.ceil(C.sqrt( [double](num_blocks) )))
    C.assert(N*N >= num_blocks, 'INTERNAL')
    return { N, N, 1 }
  end
end

Exports.thread_id     = thread_id
Exports.block_id      = block_id
Exports.num_blocks    = num_blocks
Exports.global_tid    = global_tid


-------------------------------------------------------------------------------
-- Compiling for GPUs
-------------------------------------------------------------------------------

local function get_fn_arg_syms(fn, start_at)
  start_at      = start_at or 1
  assert(terralib.isfunction(fn),'INTERNAL')
  local args    = newlist()
  local params  = fn.definition.parameters
  for k = start_at, #params do
    local avar  = params[k]
    assert(type(avar.name) == 'string', 'INTERNAL')
    args:insert( symbol(avar.type, avar.name) )
  end
  return args
end

local function batch_compile(fn_inputs)

  -- helper routine
  local timekey       = '_'..tostring(os.time())
  local function get_fn_name(base)
    -- squash every non-alphanumeric character to an underscore.
    local safe        = string.gsub(base, "[^%w_]", "_")
    return safe..timekey
  end

  local BLOCK_SIZE    = PARAMETER('GPU_BLOCK_SIZE')

  -- Package the compilation input
  local terra_fns     = {}
  local in_data       = {}
  local orig_names    = {}
  for plain_name,data in pairs(fn_inputs) do
    local name        = get_fn_name( plain_name )
    in_data[name]     = data
    orig_names[name]  = plain_name

    assert(type(data) == 'table', 'INTERNAL')
    local f           = assert(data.fn, 'INTERNAL')
    assert(f:gettype().returntype == terralib.types.unit, 'INTERNAL')
    local blocksize   = data.BLOCK_SIZE or BLOCK_SIZE

    local annotations = {
      { "maxntidx", blocksize },
      --{"maxntidy", BLOCK_DIMS[dimcount][2]},
      --{"maxntidz", BLOCK_DIMS[dimcount][3]},
      { "minctasm", 1 }
    }
    terra_fns[name]   = { kernel = f,
                          annotations = annotations }
  end
  if V.is_PTXverbose() then
    print("Compiling GPU kernels!")
  end
  local kernels, CUDA_load_fn = terralib.cudacompile(
    terra_fns,
    V.is_PTXverbose(),
    nil, -- version unspecified
    false -- defer CUDA loading
  )

  local ERR_BUF_SIZE = 2048
  local first_name   = next(orig_names)
  local cudaloader = terra()
    var error_buf : int8[ERR_BUF_SIZE]
    var retcode = CUDA_load_fn(nil,nil,error_buf,ERR_BUF_SIZE)
    if 0 ~= retcode then
      C.printf("CUDA LOAD ERROR while loading %s\n%s\n",
               first_name, error_buf)
      terralib.traceback(nil)
      C.exit(retcode)
    end
  end
  cudaloader:setname(first_name..'_cudaloader')


  -- Package the compiled code in wrapper functions
  local wrapped_fns   = {}
  for name, krn in pairs(kernels) do
    local orig_fn     = in_data[name].fn
    local orig_name   = orig_names[name]
    -- by default, we use no shared memory
    local BLOCK_SIZE  = in_data[name].BLOCK_SIZE or BLOCK_SIZE
    local SHARED_MEM  = in_data[name].SHARED_MEM or 0

    local args        = get_fn_arg_syms(orig_fn)
    local terra wrapper( n_threads : uint32, [args] )
      -- just exit if running on nothing to avoid
      -- corner case in unsigned arithmetic below
      if n_threads == 0 then return end
      var blocksize         = [uint32](BLOCK_SIZE)
      var n_blocks          = [uint32](n_threads-1)/blocksize + 1
      var shared_mem_B      = [SHARED_MEM]

      var nb_x, nb_y, nb_z  = get_grid_dimensions(n_blocks)

      var launch_params     = terralib.CUDAParams {
        nb_x,         -- #blocks in X
        nb_y,         -- #blocks in Y
        nb_z,         -- #blocks in Z
        blocksize,    -- #threads per block in X
        1,            -- #threads per block in Y
        1,            -- #threads per block in Z
        shared_mem_B, -- shared memory count in bytes
        nil,          -- this identifies the CUDA stream
      }

      -- pre-launch

      -- Launch, and check error code
      var retcode           = krn( &launch_params, [args] )
      if retcode ~= 0 then
        C.printf(["In Kernel '"..name.."',\n"..
                  "Cuda reported error %d: %s\n"],
                 retcode, C.cudaGetErrorString(retcode))
        C.exit(retcode)
        -- for now, we just explode on error
      end

      -- post-launch
    end
    wrapper:setname(name)

    wrapped_fns[orig_name] = wrapper
  end

  return wrapped_fns, cudaloader
end

local function simple_compile(fn, options)
  local name                    = fn:getname()
  local blob                    = {fn=fn}
  for k,v in pairs(options or {}) do blob[k] = v end
  local kernels, cudaloader     = batch_compile({ [name] = blob })
  return kernels[name], cudaloader
end

-- collect routines in this file here
local cudaloader_calls  = newlist()

Exports.batch_compile   = batch_compile
Exports.simple_compile  = simple_compile

-------------------------------------------------------------------------------
-- Printing
-------------------------------------------------------------------------------

local vprintf = terralib.externfunction("cudart:vprintf",
                                        {&int8,&int8} -> int)
local function build_arg_buffer(args)
  local Buf         = terralib.types.newstruct()
  for i,e in ipairs(args) do
    local typ       = e:gettype()
    if typ == float then typ = double end -- promote float types
    Buf.entries:insert{"_"..i, typ}
  end
  return quote
    var buf : Buf
    escape for i,e in ipairs(args) do
      emit quote buf.["_"..i] = e end
    end end
  in
    [&int8](&buf)
  end
end

local printf = macro(function(fmt,...)
  local buf         = build_arg_buffer({...})
  return `vprintf(fmt,buf) 
end)

Exports.printf = printf


-------------------------------------------------------------------------------
-- Math Functions
-------------------------------------------------------------------------------
-- link the bitcode for libdevice so that we can access device math functions
-- CUDA libdevice has all the math functions:
-- http://docs.nvidia.com/cuda/libdevice-users-guide/#axzz3CND85k3B
local cuda_success, cuda_version  =
  pcall(function() return cudalib.localversion() end)
local max_version, min_version    = 100000, 32 -- i.e. 3.2
if cuda_success then max_version  = cuda_version end
cuda_version                      = nil

-- determine which valid version if any exists
local device_dir_path = terralib.cudahome.."/nvvm/libdevice"
local DevDirFiles     = io.popen('ls '..device_dir_path,'r')
local fname           = DevDirFiles:read("*line")
while fname do
  local _,_,version   = fname:find("libdevice%.compute_(%d*)%.10%.bc")
  local v             = tonumber(version)
  if v >= min_version and v <= max_version then
    cuda_version = cuda_version or v
    if v > cuda_version then cuda_version = v end
  end
  fname               = DevDirFiles:read("*line")
end
DevDirFiles:close()

local externcall    = terralib.externfunction
local libdevice     = device_dir_path..
  string.format('/libdevice.compute_%d.10.bc',cuda_version)
if terralib.linkllvm then
  local llvmbc = terralib.linkllvm(libdevice)
  externcall = function(name, ftype)
    return llvmbc:extern(name, ftype)
  end
else
  terralib.linklibrary(libdevice)
end

local cbrt    = externcall("__nv_cbrt",   double -> double)
local sqrt    = externcall("__nv_sqrt",   double -> double)
local cos     = externcall("__nv_cos",    double -> double)
local acos    = externcall("__nv_acos",   double -> double)
local sin     = externcall("__nv_sin",    double -> double)
local asin    = externcall("__nv_asin",   double -> double)
local tan     = externcall("__nv_tan",    double -> double)
local atan    = externcall("__nv_atan",   double -> double)
local atan2   = externcall("__nv_atan2",  {double, double} -> double)
local log     = externcall("__nv_log",    double -> double)
local log2    = externcall("__nv_log2",   double -> double)
local pow     = externcall("__nv_pow",    {double, double} -> double)
local fmod    = externcall("__nv_fmod",   {double, double} -> double)
local floor   = externcall("__nv_floor",  double -> double)
local ceil    = externcall("__nv_ceil",   double -> double)
local fabs    = externcall("__nv_fabs",   double -> double)
local exp     = externcall("__nv_exp",    double -> double)
local exp2    = externcall("__nv_exp2",   double -> double)

local fmin    = externcall("__nv_fmin",   {double,double} -> double)
local fmax    = externcall("__nv_fmax",   {double,double} -> double)

Exports.cbrt  = cbrt
Exports.sqrt  = sqrt
Exports.cos   = cos
Exports.acos  = acos
Exports.sin   = sin
Exports.asin  = asin
Exports.tan   = tan
Exports.atan  = atan
Exports.atan2 = atan2
Exports.floor = floor
Exports.ceil  = ceil
Exports.fabs  = fabs
Exports.log   = log
Exports.log2  = log2
Exports.pow   = pow
Exports.fmod  = fmod
Exports.exp   = exp
Exports.exp2  = exp2

Exports.fmin  = fmin
Exports.fmax  = fmax


-------------------------------------------------------------------------------
-- Warp-Communication and Bit Twiddling Instructions
-------------------------------------------------------------------------------
-- NOTE: That these are not exposed outside of the utility file

-- each thread gets a single bit from each other lane
-- indicating whether the ballot instruction executed on that other lane
-- Output: a bitmask of which lanes executed the instruction
local terra warpballot_b32() : uint32
  return terralib.asm(terralib.types.uint32,
    "vote.ballot.b32 $0, 0xFFFFFFFF;","=r",false)
end
-- This instruction is now the correct safe one to use instead of warpballot
--local terra activemask_b32() : uint32
--  return terralib.asm(terralib.types.uint32,
--    "activemask.b32 $0;","=r",false)
--end

-- lanes within the warp exchange values with each other
-- Input: (1) a value to be sent to other lanes
--        (2) the index (0-31) of which other lane to read from
-- Output: the value sent by the indexed lane
-- old unsynced version is unsafe
local terra shuffle_index_b32(
  input : uint32, idx : uint32, membermask : uint32 -- ignore last arg
) : uint32
  return terralib.asm(terralib.types.uint32,
    "shfl.idx.b32 $0, $1, $2, 0x1F;","=r,r,r",false,input,idx)
end
--local terra shuffle_index_b32(
--  input : uint32, idx : uint32, membermask : uint32
--) : uint32
--  return terralib.asm(terralib.types.uint32,
--    "shfl.sync.idx.b32 $0, $1, $2, 0x1F $3;","=r,r,r,r",
--    false,input,idx,membermask)
--end

-- input:   a bit-mask
-- output:  the number of set (=1) bits in the mask
local terra popc_b32(bits : uint32) : uint32
  return terralib.asm(terralib.types.uint32,
    "popc.b32  $0, $1;","=r,r",false,bits)
end

-- input:   a bit-mask
-- output:  the bits of the mask in reverse order
local terra brev_b32(bits : uint32) : uint32
  return terralib.asm(terralib.types.uint32,
    "brev.b32  $0, $1;","=r,r",false,bits)
end

-- input:   a bit-mask
-- output:  the number of leading zeros on the input bit mask
--          starting at the highest-order bit
local terra clz_b32(bits : uint32) : uint32
  return terralib.asm(terralib.types.uint32,
    "clz.b32  $0, $1;","=r,r",false,bits)
end

--[[
  From the PTX Documentation on PTX inline assembly.
  The following codes specify how constraint letters map to register types

    "h" = .u16 reg
    "r" = .u32 reg
    "l" = .u64 reg
    "f" = .f32 reg
    "d" = .f64 reg
--]]


-------------------------------------------------------------------------------
-- Reduction Atomics...
-------------------------------------------------------------------------------

-- ADD REDUCE
local terra reduce_add_int32(address : &int32, operand : int32)
  terralib.asm(terralib.types.unit,
    "red.global.add.s32 [$0], $1;","l,r",true,address,operand)
end
local terra reduce_add_uint32(address : &uint32, operand : uint32)
  terralib.asm(terralib.types.unit,
    "red.global.add.u32 [$0], $1;","l,r",true,address,operand)
end
local terra reduce_add_int64(address : &int64, operand : int64)
  terralib.asm(terralib.types.unit,
    "red.global.add.s64 [$0], $1;","l,l",true,address,operand)
end
local terra reduce_add_uint64(address : &uint64, operand : uint64)
  terralib.asm(terralib.types.unit,
    "red.global.add.u64 [$0], $1;","l,l",true,address,operand)
end
local terra reduce_add_float(address : &float, operand : float)
  terralib.asm(terralib.types.unit,
    "red.global.add.f32 [$0], $1;","l,f",true,address,operand)
end
-- Need to use fallback for 64-bit on the dev GPU


-- MIN/MAX REDUCE
local terra reduce_max_int32(address : &int32, operand : int32)
  terralib.asm(terralib.types.unit,
    "red.global.max.s32 [$0], $1;","l,r",true,address,operand)
end
local terra reduce_max_uint32(address : &uint32, operand : uint32)
  terralib.asm(terralib.types.unit,
    "red.global.max.u32 [$0], $1;","l,r",true,address,operand)
end
local terra reduce_max_int64(address : &int64, operand : int64)
  terralib.asm(terralib.types.unit,
    "red.global.max.s64 [$0], $1;","l,l",true,address,operand)
end
local terra reduce_max_uint64(address : &uint64, operand : uint64)
  terralib.asm(terralib.types.unit,
    "red.global.max.u64 [$0], $1;","l,l",true,address,operand)
end
--local terra reduce_max_float(address : &float, operand : float)
--  terralib.asm(terralib.types.unit,
--    "red.global.max.f32 [$0], $1;","l,f",true,address,operand)
--end

local terra reduce_min_int32(address : &int32, operand : int32)
  terralib.asm(terralib.types.unit,
    "red.global.min.s32 [$0], $1;","l,r",true,address,operand)
end
local terra reduce_min_uint32(address : &uint32, operand : uint32)
  terralib.asm(terralib.types.unit,
    "red.global.min.u32 [$0], $1;","l,r",true,address,operand)
end
local terra reduce_min_int64(address : &int64, operand : int64)
  terralib.asm(terralib.types.unit,
    "red.global.min.s64 [$0], $1;","l,l",true,address,operand)
end
local terra reduce_min_uint64(address : &uint64, operand : uint64)
  terralib.asm(terralib.types.unit,
    "red.global.min.u64 [$0], $1;","l,l",true,address,operand)
end
--local terra reduce_min_float(address : &float, operand : float)
--  terralib.asm(terralib.types.unit,
--    "red.global.min.f32 [$0], $1;","l,f",true,address,operand)
--end


local terra reduce_and_b32(address : &uint32, operand : uint32)
  terralib.asm(terralib.types.unit,
    "red.global.and.b32 [$0], $1;","l,r",true,address,operand)
end
local terra reduce_or_b32(address : &uint32, operand : uint32)
  terralib.asm(terralib.types.unit,
    "red.global.or.b32 [$0], $1;","l,r",true,address,operand)
end
local terra reduce_and_b64(address : &uint64, operand : uint64)
  terralib.asm(terralib.types.unit,
    "red.global.and.b64 [$0], $1;","l,l",true,address,operand)
end
local terra reduce_or_b64(address : &uint64, operand : uint64)
  terralib.asm(terralib.types.unit,
    "red.global.or.b64 [$0], $1;","l,l",true,address,operand)
end



--[[
    op:      .and, .or, .xor,
             .add, .inc, .dec,
             .min, .max
    type:    .b32, .b64, .u32, .u64, .s32, .s64, .f32, .f64

--]]

--[[
-- doubt this will work right
local terra reduce_and_b32(address : &bool, operand : bool)
  terralib.asm(terralib.types.unit,
    "red.global.and.b32 [$0], $1;","l,r",true,address,operand)
end)

-- doubt this will work right
local terra reduce_or_b32(address : &bool, operand : bool)
  terralib.asm(terralib.types.unit,
    "red.global.or.b32 [$0], $1;","l,r",true,address,operand)
end)
--]]

-- presumably this should work too?
--local terra reduce_max_f32(address : &float, operand : float)
--  terralib.asm(terralib.types.unit,
--    "red.global.max.f32 [$0], $1;","l,f",true,address,operand)
--end
--
---- presumably this should work too?
--local terra reduce_min_f32(address : &float, operand : float)
--  terralib.asm(terralib.types.unit,
--    "red.global.min.f32 [$0], $1;","l,f",true,address,operand)
--end

--local atomic_add_float =
--  terralib.intrinsic("llvm.nvvm.atomic.load.add.f32.p0f32",
--                     {&float,float} -> {float})

local terra atomic_add_uint32(address : &uint32, operand : uint32) : uint32
  return terralib.asm(terralib.types.uint64,
    "atom.global.add.u32 $0, [$1], $2;","=r,l,r",true,address,operand)
end
local terra atomic_add_uint64(address : &uint64, operand : uint64) : uint64
  return terralib.asm(terralib.types.uint64,
    "atom.global.add.u64 $0, [$1], $2;","=l,l,l",true,address,operand)
end
--local terra reduce_add_uint64(address : &uint64, operand : uint64)
--  terralib.asm(terralib.types.unit,
--    "red.global.add.u64 [$0], $1;","l,l",true,address,operand)
--end


-------------------------------------------------------------------------------
-- Slow Atomics for GPU feature completeness
-------------------------------------------------------------------------------

local terra cas_uint64(address : &uint64, compare : uint64, value : uint64)
  return terralib.asm(terralib.types.uint64,
    "atom.global.cas.b64 $0, [$1], $2, $3;",
    "=l,l,l,l",true,address,compare,value)
end

local terra cas_uint32(address : &uint32, compare : uint32, value : uint32)
  return terralib.asm(terralib.types.uint32,
    "atom.global.cas.b32 $0, [$1], $2, $3;",
    "=r,l,r,r",true,address,compare,value)
end

local function generate_slow_atomic_64 (op, typ)
  return terra (address : &typ, operand : typ)
    var old : typ = @address
    var assumed : typ
    var new     : typ

    var new_b     : &uint64 = [&uint64](&new)
    var assumed_b : &uint64 = [&uint64](&assumed)
    var res       :  uint64

    var mask = false
    repeat
      if not mask then
        assumed = old
        new     = op(assumed,operand)
        res     = cas_uint64([&uint64](address), @assumed_b, @new_b)
        old     = @[&typ](&res)
        mask    = assumed == old
      end
    until mask
  end
end

local function generate_slow_atomic_32 (op, typ)
  return terra (address : &typ, operand : typ)
    var old : typ = @address
    var assumed   : typ
    var new       : typ

    var new_b     : &uint32 = [&uint32](&new)
    var assumed_b : &uint32 = [&uint32](&assumed)
    var res       :  uint32

    var mask = false
    repeat
      if not mask then
        assumed = old
        new     = op(assumed,operand)
        res     = cas_uint32([&uint32](address), @assumed_b, @new_b)
        old     = @[&typ](&res)
        mask    = assumed == old
      end
    until mask
  end
end

-- Operator quotes
local mul = macro(function(a, b) return `a*b end)
local add = macro(function(a, b) return `a+b end)
local div = macro(function(a, b) return `a/b end)
local max = macro(function(a, b)
  local atyp, btyp = a:gettype(), b:gettype()
  assert(atyp == btyp)
  return quote
    var max : atyp
    if a > b then max = a
    else          max = b
    end
  in
    max
  end
end)
local min = macro(function(a, b)
  local atyp, btyp = a:gettype(), b:gettype()
  assert(atyp == btyp)
  return quote 
    var min : atyp
    if a < b then min = a
    else          min = b
    end
  in
    min
  end
end)


-- Intrinsic atomic reductions:
-- Addition
Exports.reduce_add_int32        = reduce_add_int32
Exports.reduce_add_uint32       = reduce_add_uint32
Exports.reduce_add_int64        = reduce_add_int64
Exports.reduce_add_uint64       = reduce_add_uint64
Exports.reduce_add_float        = reduce_add_float
Exports.reduce_add_double       = generate_slow_atomic_64(add,double)

Exports.reduce_max_int32        = reduce_max_int32
Exports.reduce_max_uint32       = reduce_max_uint32
Exports.reduce_max_int64        = reduce_max_int64
Exports.reduce_max_uint64       = reduce_max_uint64
Exports.reduce_max_float        = generate_slow_atomic_32(max,float)
Exports.reduce_max_double       = generate_slow_atomic_64(max,double)

Exports.reduce_min_int32        = reduce_min_int32
Exports.reduce_min_uint32       = reduce_min_uint32
Exports.reduce_min_int64        = reduce_min_int64
Exports.reduce_min_uint64       = reduce_min_uint64
Exports.reduce_min_float        = generate_slow_atomic_32(min,float)
Exports.reduce_min_double       = generate_slow_atomic_64(min,double)

Exports.reduce_and_b32          = reduce_and_b32
Exports.reduce_or_b32           = reduce_or_b32
Exports.reduce_and_b64          = reduce_and_b64
Exports.reduce_or_b64           = reduce_or_b64


--Exports.reduce_add_int32        = reduce_add_int32
--Exports.atomic_add_float        = atomic_add_float
--Exports.reduce_max_int32        = reduce_max_int32
--Exports.reduce_min_int32        = reduce_min_int32
--Exports.reduce_add_int32        = reduce_add_int32
--Exports.atomic_max_uint32       = reduce_max_uint32
--Exports.reduce_min_uint32       = reduce_min_uint32
--Exports.reduce_add_uint32       = reduce_add_uint32
--Exports.reduce_and_b32          = reduce_and_b32
--Exports.reduce_or_b32           = reduce_or_b32

--Exports.reduce_add_uint64       = reduce_add_uint64

-- Slow operations:
Exports.atomic_add_uint32       = atomic_add_uint32
Exports.atomic_add_uint64       = atomic_add_uint64
--Exports.atomic_add_double_SLOW  = generate_slow_atomic_64(add,double)

Exports.atomic_mul_float_SLOW   = generate_slow_atomic_32(mul,float)
Exports.atomic_mul_double_SLOW  = generate_slow_atomic_64(mul,double)
Exports.atomic_mul_int32_SLOW   = generate_slow_atomic_32(mul,int32)
Exports.atomic_mul_uint32_SLOW  = generate_slow_atomic_32(mul,uint32)
Exports.atomic_mul_int64_SLOW   = generate_slow_atomic_64(mul,int64)
Exports.atomic_mul_uint64_SLOW  = generate_slow_atomic_64(mul,uint64)

--Exports.atomic_div_float_SLOW   = generate_slow_atomic_32(div,float)
--Exports.atomic_div_double_SLOW  = generate_slow_atomic_64(div,double)

--Exports.atomic_min_uint64_SLOW  = generate_slow_atomic_64(min,uint64)
--Exports.atomic_min_double_SLOW  = generate_slow_atomic_64(min,double)
--Exports.atomic_min_float_SLOW   = generate_slow_atomic_32(min,float)

--Exports.atomic_max_uint64_SLOW  = generate_slow_atomic_64(max,uint64)
--Exports.atomic_max_double_SLOW  = generate_slow_atomic_64(max,double)
--Exports.atomic_max_float_SLOW   = generate_slow_atomic_32(max,float)

Exports.clz_b32                 = clz_b32

-------------------------------------------------------------------------------
-- Other exposed GPU instructions
-------------------------------------------------------------------------------

-- impose a barrier to synchronize within blocks,
-- but not across different blocks
local block_barrier     = macro(function()
  return quote cudalib.nvvm_barrier0() end
end)

local terra cuda_trap()
  @[&uint32](0) = 64 -- this should cause a panic...
  --terralib.asm(terralib.types.unit, "trap;", "", false)
end

Exports.block_barrier   = block_barrier
Exports.trap            = cuda_trap

-------------------------------------------------------------------------------
-- Some API Wrappers
-------------------------------------------------------------------------------

local cuda_check = macro(function(call)
  return quote
    var retcode = [call]
    if retcode ~= 0 then
      C.printf("CUDA Reported ERROR %d: %s\n",
               retcode, C.cudaGetErrorString(retcode))
      -- for now just explode
      C.exit(retcode)
    end
  end
end)

--local terra cuda_peek_at_last_error()
--  cuda_error_checking(C.cudaPeekAtLastError())
--end

--local sync_temp_wrapper =
--        terralib.externfunction("cudaThreadSynchronize", {} -> int)
local device_sync =
  terralib.externfunction("cudaDeviceSynchronize", {} -> int)
local terra cuda_sync()
  --var res = sync_temp_wrapper()
  cuda_check( device_sync() )
end

Exports.sync        = cuda_sync


-------------------------------------------------------------------------------
-- GPU Memory Management
-------------------------------------------------------------------------------

local terra memcpy_from_gpu(cpu_dst : &opaque, gpu_src : &opaque, n : uint64)
  cuda_check( C.cudaMemcpy(cpu_dst, gpu_src, n, C.cudaMemcpyDeviceToHost) )
end
local terra memcpy_to_gpu(gpu_dst : &opaque, cpu_src : &opaque, n : uint64)
  cuda_check( C.cudaMemcpy(gpu_dst, cpu_src, n, C.cudaMemcpyHostToDevice) )
end
local terra memcpy_on_gpu(dst : &opaque, src : &opaque, n : uint64)
  cuda_check( C.cudaMemcpy(dst, src, n, C.cudaMemcpyDeviceToDevice) )
end

local terra gpu_malloc(size : uint64)
  var r : &opaque
  cuda_check( C.cudaMalloc(&r, size) )
  return r
end
local terra gpu_free(ptr : &opaque)
  cuda_check( C.cudaFree(ptr) )
end

local terra gpu_memzero( ptr : &opaque, size : uint64 )
  cuda_check( C.cudaMemset(ptr, 0, size) )
end

Exports.malloc          = gpu_malloc
Exports.free            = gpu_free
Exports.memzero         = gpu_memzero
Exports.memcpy_from_gpu = memcpy_from_gpu
Exports.memcpy_to_gpu   = memcpy_to_gpu
Exports.memcpy_on_gpu   = memcpy_on_gpu


-------------------------------------------------------------------------------
-- Random Number Generation on the GPU
-------------------------------------------------------------------------------
-- Taken from: http://www.reedbeta.com/blog/2013/01/12/quick-and-easy-gpu-random-numbers-in-d3d11/

local RAND_MAX          = PARAMETER('GPU_RAND_MAX')
local RAND_BUFSIZE      = PARAMETER('GPU_RAND_BUF_SIZE')

local struct GPU_RandBuffer {
  _buf   : &uint32
}

-- Note: this is a GPU kernel, compiled below
local terra init_rand_buffer( buffer : GPU_RandBuffer, seed : uint32 )
  var tid = [uint32](global_tid())
  if tid < RAND_BUFSIZE then -- mask the final positions...
    -- compute a seed from the thread id and the global seed value
    seed = tid + seed
    -- do the wang hash to get more uncorrelated initial seed values
    seed = (seed ^ 61) ^ (seed >> 16)
    seed = seed * 9
    seed = seed ^ (seed >> 4)
    seed = seed * 0x27d4eb2d
    seed = seed ^ (seed >> 15)

    -- then write out the initialized seed into the buffer
    buffer._buf[tid]  = seed
  end
end
local init_rand_buffer, load_init_rand = simple_compile(init_rand_buffer)
--init_rand_buffer    = batch_compile({
--                        init_rand_buffer = { fn = init_rand_buffer }
--                      }).init_rand_buffer
cudaloader_calls:insert(load_init_rand)

local terra NewRandBuffer( seed : uint32 ) : GPU_RandBuffer
  var buf : GPU_RandBuffer
  buf._buf = [&uint32]( gpu_malloc(RAND_BUFSIZE * sizeof(uint32)) )
  init_rand_buffer(RAND_BUFSIZE, buf, seed)
  return buf
end

terra GPU_RandBuffer:destroy() -- run on CPU
  gpu_free(self._buf)
end

GPU_RandBuffer.methods.rand = macro(function(self, tid) -- run on GPU
  -- XOR-shift PRNG from same source as above
  return quote
    var tidmod          = [uint32](tid) % [uint32](RAND_BUFSIZE)
    var rng_state       = self._buf[tidmod]
    rng_state           = rng_state ^ (rng_state << 13)
    rng_state           = rng_state ^ (rng_state >> 17)
    rng_state           = rng_state ^ (rng_state << 5)
    self._buf[tidmod]   = rng_state
  in
    rng_state
  end
end)

Exports.RandBuffer      = GPU_RandBuffer
Exports.NewRandBuffer   = NewRandBuffer


-------------------------------------------------------------------------------
-- Global Reductions on the GPU
-------------------------------------------------------------------------------

local ReductionObj      = {}
ReductionObj.__index    = ReductionObj

local function NewReductionObj(args)
  assert(terralib.types.istype(args.type), 'INTERNAL: expect terra type')
  local typ             = args.type
  -- optional argument
  local blocksize       = args.BLOCK_SIZE or PARAMETER('GPU_BLOCK_SIZE')
  local n_shared        = blocksize
  -- the following is a Terra expression quote that should only be
  -- included inside GPU kernels
  local sharedmem_ptr   = cudalib.sharedmemory(typ, n_shared)

  local obj = setmetatable({
    _type               = typ,
    _blocksize          = blocksize,
    _n_shared           = n_shared,
    _sharedmem_ptr      = sharedmem_ptr,
    _shared_bytes       = terralib.sizeof(typ) * n_shared,

    _reduce_id          = assert(args.reduce_id, 'INTERNAL'),
    _reduce_binop       = assert(args.reduce_binop, 'INTERNAL'),
    _reduce_atomic      = assert(args.gpu_reduce_atomic, 'INTERNAL'),

  }, ReductionObj)

  return obj
end

function ReductionObj:type()              return self._type           end
function ReductionObj:sharedmem_bytes()   return self._shared_bytes   end
function ReductionObj:n_shared()          return self._n_shared       end
function ReductionObj:shared_ptr()        return self._sharedmem_ptr  end

function ReductionObj:init_sharedmem(tid)
  return quote if tid < [self._n_shared] then
    [self._sharedmem_ptr][tid] = [self._reduce_id]
  end end
end

function ReductionObj:reduce_block_sharedmem(tid, result_ptr)
  -- start with a barrier to ensure that written shared memory
  -- values are visible across the whole block
  local code = quote block_barrier() end
  local step = self._n_shared
  -- tree of reductions in shared memory
  while step > 1 do
    step = step / 2
    code = quote
      [code]
      if tid < step then
        var result  = self._reduce_binop([self._sharedmem_ptr][tid],
                                         [self._sharedmem_ptr][tid+step])
        -- unsure if we need a volatile write with barrier below
        terralib.attrstore(&[self._sharedmem_ptr][tid],
                           result, {isvolatile=true})
      end
      block_barrier() -- the block needs to always sync after each shared
                      -- memory write...
    end
  end
  -- finally, reduce the result into the specified memory address
  code = quote
    [code]
    if tid == 0 then
      self._reduce_atomic( result_ptr, [self._sharedmem_ptr][tid] )
    end
  end
  return code
end

Exports.NewReductionObj     = NewReductionObj


-------------------------------------------------------------------------------
-- Sorting on the GPU
-------------------------------------------------------------------------------


-- hack for now...
local sort_tmp_alloc_Nseg   = global(uint64, 0, 'sort_tmp_alloc_Nseg')
local sort_tmp_alloc_N      = global(uint64, 0, 'sort_tmp_alloc_N')
local sort_tmp_alloc_B      = global(uint64, 0, 'sort_tmp_alloc_B')
local sort_tmp_alloc_B_all  = global(uint64, 0, 'sort_tmp_alloc_B_all')
local sort_tmp_buffer       = global(&opaque, nil, 'sort_tmp_buffer')

local terra extra_space(N : uint64)
  return 512*((N*4 - 1)/512 + 1)
end
local terra max_space(N : uint64, Nseg : uint64)
  var n_bytes0 : uint64 = 0
  var n_bytes1 : uint64 = 0
  var n_bytes2 : uint64 = 0
  cuda_check( CUB.cub_wrap_get_sort64_bytes(N, &n_bytes0) )
  cuda_check( CUB.cub_wrap_get_prefix32_bytes(N, &n_bytes1) )
  cuda_check( CUB.cub_wrap_get_seg_sort32_bytes(N, Nseg, &n_bytes2))
  var maxB = n_bytes0
  if n_bytes1 > maxB then maxB = n_bytes1 end
  if n_bytes2 > maxB then maxB = n_bytes2 end
  return maxB
end

local terra reserve_space(N : uint64, Nseg : uint64)
  if N > sort_tmp_alloc_N or Nseg > sort_tmp_alloc_Nseg then
    sort_tmp_alloc_N    = N
    sort_tmp_alloc_Nseg = Nseg
    sort_tmp_alloc_B    = max_space(N, Nseg)
    --align maxB...
    var allB            = 512*((sort_tmp_alloc_B - 1)/512 + 1)
                        + 2*extra_space(N)
    sort_tmp_alloc_B_all = allB
    if sort_tmp_buffer ~= nil then
      gpu_free(sort_tmp_buffer)
    end
    sort_tmp_buffer = gpu_malloc(sort_tmp_alloc_B_all)
  end
end

local terra ALLOC_EXTRAu32(N : uint64) : {&uint32, &uint32}
  var end_ptr = [&uint8](sort_tmp_buffer) + sort_tmp_alloc_B_all
  var nB      = extra_space(N)
  var X       = end_ptr - nB
  var Y       = X - nB
  return [&uint32](X), [&uint32](Y)
end

local terra sort64(N : uint64, maxBits : int32,
                   key_in : &uint64, key_out : &uint64,
                   val_in : &uint32, val_out : &uint32
)
  reserve_space(N,1)
  cuda_check( CUB.cub_wrap_do_sort64( N, maxBits,
                                      sort_tmp_buffer, sort_tmp_alloc_B,
                                      key_in, key_out, val_in, val_out ) )
end
local terra sort32(N : uint64, maxBits : int32,
                   key_in : &uint32, key_out : &uint32,
                   val_in : &uint32, val_out : &uint32
)
  reserve_space(N,1)
  cuda_check( CUB.cub_wrap_do_sort32( N, maxBits,
                                      sort_tmp_buffer, sort_tmp_alloc_B,
                                      key_in, key_out, val_in, val_out ) )
end
local terra seg_sort32( N : uint64, maxBits : int32,
                        Nseg : uint64, segments : &uint32,
                        key_in : &uint32, key_out : &uint32,
                        val_in : &uint32, val_out : &uint32
)
  reserve_space(N,Nseg)
  cuda_check(CUB.cub_wrap_do_seg_sort32(
    N, maxBits, sort_tmp_buffer, sort_tmp_alloc_B,
    Nseg, segments, key_in, key_out, val_in, val_out
  ))
end

Exports.sort32      = sort32
Exports.sort64      = sort64
Exports.seg_sort32  = seg_sort32



local terra prefix32(N : uint64, buf_in : &uint32, buf_out : &uint32)
  reserve_space(N,1)
  cuda_check( CUB.cub_wrap_do_prefix32( N, sort_tmp_buffer, sort_tmp_alloc_B,
                                        buf_in, buf_out ) )
end

Exports.prefix32 = prefix32

--

-- Woah, this makes a big difference in execution time
local terra read_nc_uint32(address : &uint32) : uint32
  return terralib.asm(terralib.types.uint32,
    "ld.global.nc.u32 $0, [$1];","=r,l",true,address)
end
local terra sort_helper0( N : uint64, idx : &uint32,
                          key1_in : &uint32, key1_out : &uint32
)
  var tid = [uint32](global_tid())
  if tid < N then
    var i           = idx[tid]
    -- gather key1 values
    key1_out[tid]   = read_nc_uint32( key1_in + i )
  end
end
local terra gen_segments( Nseg : int32, N : uint64,
                          key0 : &uint32, seg_out : &uint32
)
  var tid = [uint32](global_tid())
  if tid < Nseg then
    -- special case: empty key array
    if N == 0 then seg_out[tid] = 0; return end
    
    -- usual case
    var id          = tid
    -- binary search for the start of segment id
    var lo, hi      = 0, N
    -- loop invariant:
    --      (key0[lo] <  id  or  lo == 0)
    --  and (key0[hi] >= id  or  hi == N)
    while lo+1 < hi do -- narrow down till lo+1 == hi
      var mid       = (hi+lo)/2
      var mval      = key0[mid]
      if id > mval then
        lo = mid
      else
        hi = mid
      end
    end
    if lo == 0 then -- in this case, we might need to decrement hi
      if id <= key0[0] then hi = 0 end
    end
    seg_out[id] = hi
  end
end
local sort_helper0, sort_helper0_init = simple_compile(sort_helper0)
local gen_segments, gen_segments_init = simple_compile(gen_segments)
cudaloader_calls:insert(sort_helper0_init)
cudaloader_calls:insert(gen_segments_init)
--

-- sorting according to a double-key
local terra sort_32_32( N : uint64, maxKey : int32,
                        key0_in : &uint32, key0_out : &uint32,
                        key1_in : &uint32, key1_out : &uint32,
                        seg_out  : &uint32,
                        idxvals : &uint32 -- should be 0, 1, .., N to begin
)
  reserve_space(N, maxKey)
  var idx_tmp, key_tmp      = ALLOC_EXTRAu32(N)
  var maxBits               = [int]( C.ceil( C.log2(maxKey) ) )
  -- sort on lower order key first
  sort32( N, maxBits, key1_in, key1_out, idxvals, idx_tmp )
  -- and gather the key0 results
  sort_helper0( N, N, idx_tmp, key0_in, key1_out )
  sort32( N, maxBits, key1_out, key0_out, idx_tmp, idxvals )
  sort_helper0( N, N, idxvals, key1_in, key1_out )
  gen_segments( maxKey, maxKey, N, key0_out, seg_out )
end
--]]

Exports.sort_32_32    = sort_32_32
Exports.gen_segments  = gen_segments


local terra datashuffle_helper( N   : uint64,  n_bytes : uint32,
                                idx : &uint32,
                                din_ : &opaque, dout_ : &opaque )
  var tid         = [uint32](global_tid())
  var din         = [&uint32](din_)
  var dout        = [&uint32](dout_)
  var N_u32       = n_bytes >> 2
  if tid < N*N_u32 then
    var off       = tid % N_u32
    var write_i   = tid / N_u32
    var read_i    = idx[write_i]
    var val       = din[N_u32 * read_i + off]
    dout[N_u32 * write_i + off] = val
  end
end
local datashuffle_helper,datashuffle_init = simple_compile(datashuffle_helper)
cudaloader_calls:insert(datashuffle_init)

local terra datashuffle( N   : uint64,  n_bytes : uint32,
                         idx : &uint32,
                         din : &opaque, dout : &opaque )
  C.assert(n_bytes % 4 == 0,
           'INTERNAL: simplifying; assuming 4-byte values on gpu')
  var n_thread    = N * (n_bytes / 4)
  datashuffle_helper(n_thread, N, n_bytes, idx, din, dout)
end

Exports.datashuffle   = datashuffle

-------------------------------------------------------------------------------
-- Write Buffer Control
-------------------------------------------------------------------------------

-- recipe runs in order to reduce traffic to one global access per warp
local reserve_idx = macro(function(tid, write_idx_ptr)
  assert(write_idx_ptr:gettype() == &uint32, 'INTERNAL: bad ptr type')
  return quote
    -- first, figure out which threads are running this code
    var activemask    = warpballot_b32() --activemask_b32()
    -- how many threads is that?
    var write_count   = popc_b32(activemask)
    -- this mask is 0 at this thread, and 1 at every lower-order bit
    var lower_mask    = [uint32]( ([uint64](1) << (tid%32)) - 1 )
    -- the number of lower-id active threads
    var active_lower  = popc_b32(activemask and lower_mask)
    -- even if this thread isn't the leader, figure out which one is
    -- do this by finding the lowest-order 1 bit in the ballot
    var leader_id     = clz_b32(brev_b32(activemask))
    var is_leader     = (active_lower == 0)

    var start_idx : uint32 = 0
    if is_leader then -- reserve space for the whole warp
      start_idx       = atomic_add_uint32(write_idx_ptr, write_count)
      --printf("[%4d] leader_id %4d writes %4d mask %08x start %4d\n",
      --       tid, leader_id, write_count, activemask, start_idx)
    end
    -- scatter the starting value to all active threads
    start_idx         = shuffle_index_b32(start_idx, leader_id, activemask)
  in
    -- finally, return an index telling the thread where to write to
    start_idx + active_lower
  end
end)

-- unreliable, so instead
reserve_idx = macro(function(tid, write_idx_ptr)
  assert(write_idx_ptr:gettype() == &uint32, 'INTERNAL: bad ptr type')
  return quote
    var start_idx = atomic_add_uint32(write_idx_ptr, 1)
    --printf("[%4d] start %4d\n", tid, start_idx)
  in
    start_idx
  end
end)


-- recipe decrements the value once for every thread that executes it
-- but coallesces those decrements for efficiency purposes
local warp_dec_32 = macro(function(tid, write_idx_ptr)
  assert(write_idx_ptr:gettype() == &uint32, 'INTERNAL: bad ptr type')
  return quote
    -- first, figure out which threads are running this code
    var activemask    = warpballot_b32() --activemask_b32()
    -- how many threads is that?
    var dec_count     = popc_b32(activemask)
    -- this mask is 0 at this thread, and 1 at every lower-order bit
    var lower_mask    = [uint32]( ([uint64](1) << (tid%32)) - 1 )
    -- the number of lower-id active threads
    var active_lower  = popc_b32(activemask and lower_mask)
    -- determine whether this thread is the leader
    -- even if this thread isn't the leader, figure out which one is
    -- do this by finding the lowest-order 1 bit in the ballot
    --var leader_id     = clz_b32(brev_b32(activemask))
    var is_leader     = (active_lower == 0)

    if is_leader then -- decrement on behalf of the warp
      reduce_add_uint32(write_idx_ptr, -dec_count)
    end
  end
end)

-- unreliable, so instead
warp_dec_32 = macro(function(tid, write_idx_ptr)
  assert(write_idx_ptr:gettype() == &uint32, 'INTERNAL: bad ptr type')
  return quote
    reduce_add_uint32(write_idx_ptr, -1)
  end
end)


Exports.reserve_idx   = reserve_idx
Exports.warp_dec_32   = warp_dec_32




-------------------------------------------------------------------------------
-- Collection of CUDA loading routines...
-------------------------------------------------------------------------------

local terra init_cudakernels() escape
  for _,kf in ipairs(cudaloader_calls) do emit quote
    kf()
  end end
end end

Exports.init_cudakernels  = init_cudakernels




