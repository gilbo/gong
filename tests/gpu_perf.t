--import 'gong'
local test    = require 'tests.test'

local GPU     = require 'gong.src.gpu_util'

if not GPU.malloc then
  print("GPU not enabled")
  return
end

--
--  This series of tests is really about getting some numbers for
--  doing back of the envelope reasoning about costs of operations
--

local newlist = terralib.newlist

------------------------------------------------------------------------------


local function empty(n)
  local e=newlist()
  for k=1,n do e:insert(' ') end
  return e:concat()
end
local function gpu_time(name, fn, ...)
  local start = terralib.currenttimeinseconds()
  fn(...)
  GPU.sync()
  local stop  = terralib.currenttimeinseconds()
  local report = "runtime for "..name..":  "
  report = report .. empty(math.max(0, 50 - #report))
  print (report..((stop-start) * 1e3).." ms")
end
local function run_n_times(n, fn, ...)
  for k=1,n do
    fn(...)
  end
end

local terra inc_buf( N : uint32, buf : &uint32 )
  var tid = [uint32](GPU.global_tid())
  if tid < N then
    buf[tid] = buf[tid] + 1
  end
end
inc_buf = GPU.simple_compile(inc_buf)

local terra raw_reduce( N : uint32, buf : &uint32, res : &uint32 )
  var tid = [uint32](GPU.global_tid())
  if tid < N then
    var c = buf[tid]
    -- atomic reduce!
    GPU.reduce_add_uint32(res, c)
  end
end
raw_reduce = GPU.simple_compile(raw_reduce)
-- using atomic instead of reduction doesn't seem to make any
-- performance difference on my dev hardware

local REDBLOCK = 128
local redobj = GPU.NewReductionObj{
  type              = uint32,
  BLOCK_SIZE        = REDBLOCK,
  reduce_id         = (`[uint32](0)),
  reduce_binop      = macro(function(x,y) return (`x + y) end),
  gpu_reduce_atomic = GPU.reduce_add_uint32,
}
local terra block_reduce( N : uint32, buf : &uint32, res : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  var local_tid   = [uint32](GPU.thread_id())
  if tid < N then
    var c = buf[tid]
    -- atomic reduce!
    [ redobj:shared_ptr() ][local_tid] = c
    [ redobj:reduce_block_sharedmem( local_tid, res ) ]
  end
end
block_reduce = GPU.simple_compile(block_reduce, {
  BLOCK_SIZE = REDBLOCK,
  SHARED_MEM = REDBLOCK,
})

local terra write_rand( N : uint32, rand : GPU.RandBuffer, buf : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var r = rand:rand(tid)
    buf[tid] = r
  end
end
write_rand = GPU.simple_compile(write_rand)


local terra reserve_test_kern( N : uint32, rand : GPU.RandBuffer,
                               buf_in : &uint32, out_count : &uint32,
                               OUT_MAX : uint32, buf_out : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var val   = buf_in[tid]
    var n_out = (rand:rand(tid))%10
    -- write the output to the given locations...
    for k=0,n_out do
      var idx = GPU.reserve_idx(  tid, out_count  )
      if idx >= OUT_MAX then GPU.trap() end
      buf_out[idx] = val
    end
  end
end
reserve_test_kern = GPU.simple_compile(reserve_test_kern)

local function reserve_test(n, N, rand, buf_in,
                            out_count, OUT_MAX, buf_out)
  GPU.memzero(out_count, 4)
  reserve_test_kern(  n,  N,rand,buf_in,out_count,OUT_MAX,buf_out  )
end


local terra randkeygen_kern( N : uint32, rand : GPU.RandBuffer,
                             N_key : uint32, count_buf : &uint32,
                             OUT_MAX : uint32,
                             out_count : &uint32, buf_out : &uint32
                            )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var n_out = (rand:rand(tid))%10
    for k=0,n_out do
      var key   = (rand:rand(tid))%N_key
      GPU.reduce_add_uint32(count_buf+key, 1)
      var idx = GPU.reserve_idx(  tid, out_count  )
      if idx >= OUT_MAX then GPU.trap() end
      buf_out[idx] = key
    end
  end
end
randkeygen_kern = GPU.simple_compile(randkeygen_kern)
--[[
local terra prefix_sum_1_kern( --N : uint32, --rand : GPU.RandBuffer,
                               N_key : uint32, count_buf : &uint32,
                                               off_buf : uint32 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N_key then
    var c         = count_buf[tid]
    var val   = buf_in[tid]
    var n_out = (rand:rand(tid))%10
    -- write the output to the given locations...
    for k=0,n_out do
      var idx = GPU.reserve_idx(  tid, out_count  )
      if idx >= OUT_MAX then GPU.trap() end
      buf_out[idx] = val
    end
  end
end
prefix_sum_1_kern = GPU.simple_compile(prefix_sum_1_kern)
--]]

local merge_cache = {}
local function merge_test(n, N, rand, N_key, OUT_MAX)
  if not merge_cache.N_key then
    merge_cache.N_key     = N_key
    merge_cache.OUT_MAX   = OUT_MAX
    merge_cache.count_buf = terralib.cast(&uint32, GPU.malloc(4*N_key))
    merge_cache.out_count = terralib.cast(&uint32, GPU.malloc(4))
    merge_cache.out_tmp   = terralib.cast(&uint32, GPU.malloc(4*OUT_MAX))
  end
  assert(N_key == merge_cache.N_key, 'Nkey mismatch')
  assert(OUT_MAX == merge_cache.OUT_MAX, 'OUT_MAX mismatch')
  local count_buf     = merge_cache.count_buf
  local out_count     = merge_cache.out_count
  local out_tmp       = merge_cache.out_tmp

  GPU.memzero(count_buf, 4*N_key)
  GPU.memzero(out_count, 4)
  randkeygen_kern(  n,  N, rand, N_key, count_buf, OUT_MAX,
                           out_count, out_tmp )
end



local N       = 65536 * 16
local buffer  = terralib.cast(&uint32, GPU.malloc(4*N))
local OUTSIZE = N*8
local N_key   = N
local buf_out = terralib.cast(&uint32, GPU.malloc(4*OUTSIZE))
local globreg = terralib.cast(&uint32, GPU.malloc(8))
local n_out   = terralib.cast(&uint32, GPU.malloc(4))
local randbuf = GPU.NewRandBuffer(0)

GPU.memzero(globreg, 8)
GPU.memzero(buffer, N*4)
gpu_time('inc_buf', inc_buf, N, N, buffer)
gpu_time('raw_reduce', raw_reduce, N, N, buffer, globreg)
gpu_time('block_reduce', block_reduce, N, N, buffer, globreg)

local globval = (terra()
  var g : uint32 = 0
  GPU.memcpy_from_gpu(&g, globreg, 4)
  return g
end)()
print('globval... ', globval)

gpu_time('write_rand', write_rand, N, N, randbuf, buffer)
gpu_time('reserve_test', reserve_test, N, N, randbuf, buffer,
                                          n_out, OUTSIZE, buf_out)
gpu_time('reserve_test', reserve_test, N, N, randbuf, buffer,
                                          n_out, OUTSIZE, buf_out)

print('n out... ', (terra()
  var g : uint32 = 0
  GPU.memcpy_from_gpu(&g, n_out, 4)
  return g
end)())

gpu_time('merge_test', merge_test, N, N, randbuf, N_key, OUTSIZE)

gpu_time('inc_buf', inc_buf, N, N, buffer)
gpu_time('inc_buf', inc_buf, N, N, buffer)
gpu_time('inc_buf x 100', run_n_times, 100, inc_buf, N, N, buffer)
gpu_time('inc_buf', inc_buf, N, N, buffer)

gpu_time('raw_reduce', raw_reduce, N, N, buffer, globreg)
gpu_time('raw_reduce', raw_reduce, N, N, buffer, globreg)
gpu_time('raw_reduce x 100', run_n_times, 100,
                       raw_reduce, N, N, buffer, globreg)
gpu_time('raw_reduce', raw_reduce, N, N, buffer, globreg)

gpu_time('block_reduce', block_reduce, N, N, buffer, globreg)
gpu_time('block_reduce', block_reduce, N, N, buffer, globreg)
gpu_time('block_reduce x 100', run_n_times, 100,
                         block_reduce, N, N, buffer, globreg)
gpu_time('block_reduce', block_reduce, N, N, buffer, globreg)

gpu_time('reserve_test', reserve_test, N, N, randbuf, buffer,
                                          n_out, OUTSIZE, buf_out)
gpu_time('reserve_test', reserve_test, N, N, randbuf, buffer,
                                          n_out, OUTSIZE, buf_out)
gpu_time('reserve_test x 100', run_n_times, 100,
                         reserve_test, N, N, randbuf, buffer,
                                          n_out, OUTSIZE, buf_out)
gpu_time('reserve_test', reserve_test, N, N, randbuf, buffer,
                                          n_out, OUTSIZE, buf_out)

gpu_time('merge_test', merge_test, N, N, randbuf, N_key, OUTSIZE)
gpu_time('merge_test', merge_test, N, N, randbuf, N_key, OUTSIZE)
gpu_time('merge_test x 100', run_n_times, 100,
                       merge_test, N, N, randbuf, N_key, OUTSIZE)
gpu_time('merge_test', merge_test, N, N, randbuf, N_key, OUTSIZE)

randbuf:destroy()
GPU.free(n_out)
GPU.free(globreg)
GPU.free(buf_out)
GPU.free(buffer)

