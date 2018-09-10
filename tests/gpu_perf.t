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

GPU.init_cudakernels()

local function compile(...)
  local fn, loader = GPU.simple_compile(...)
  loader()
  return fn
end

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
inc_buf = compile(inc_buf)

local terra raw_reduce( N : uint32, buf : &uint32, res : &uint32 )
  var tid = [uint32](GPU.global_tid())
  if tid < N then
    var c = buf[tid]
    -- atomic reduce!
    GPU.reduce_add_uint32(res, c)
  end
end
raw_reduce = compile(raw_reduce)
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
block_reduce = compile(block_reduce, {
  BLOCK_SIZE = REDBLOCK,
  SHARED_MEM = REDBLOCK,
})

local terra write_rand( N : uint32, rand : GPU.RandBuffer, buf : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var r = rand:rand(tid) % N
    buf[tid] = r
  end
end
write_rand = compile(write_rand)



-- Woah, this makes a big difference in execution time
local terra read_nc_uint32(address : &uint32) : uint32
  return terralib.asm(terralib.types.uint32,
    "ld.global.nc.u32 $0, [$1];","=r,l",true,address)
end

local terra unsafe_gather( N : uint32, rand : GPU.RandBuffer,
                           buf_in : &uint32, buf_out : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var rand_id = rand:rand(tid) % N
    var val     = read_nc_uint32(buf_in+rand_id) --buf_in[rand_id]
    buf_out[tid] = val
  end
end
unsafe_gather = compile(unsafe_gather)

local terra unsafe_scatter( N : uint32, rand : GPU.RandBuffer,
                            buf_in : &uint32, buf_out : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var rand_id = rand:rand(tid) % N
    var val     = buf_in[tid]
    buf_out[rand_id] = val
  end
end
unsafe_scatter = compile(unsafe_scatter)

local terra safe_scatter( N : uint32, rand : GPU.RandBuffer,
                            buf_in : &uint32, buf_out : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var rand_id = rand:rand(tid) % N
    var val     = buf_in[tid]
    GPU.reduce_add_uint32( &(buf_out[rand_id]), val )
  end
end
safe_scatter = compile(safe_scatter)

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
reserve_test_kern = compile(reserve_test_kern)

local function reserve_test(n, N, rand, buf_in,
                            out_count, OUT_MAX, buf_out)
  GPU.memzero(out_count, 4)
  reserve_test_kern(  n,  N,rand,buf_in,out_count,OUT_MAX,buf_out  )
end


local terra randkeygen_kern( N : uint32, rand : GPU.RandBuffer,
                             N_key : uint32, count_buf : &uint32,
                             OUT_MAX : uint32,
                             out_count : &uint32, buf_out : &uint32,
                             off_out : &uint32
                            )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var n_out = 1--(rand:rand(tid))%10
    for k=0,n_out do
      var key   = (rand:rand(tid))%N_key
      var off   = GPU.atomic_add_uint32(count_buf+key, 1)
      --GPU.reduce_add_uint32(count_buf+key, 1)
      var idx = GPU.reserve_idx(  tid, out_count  )
      if idx >= OUT_MAX then GPU.trap() end
      buf_out[idx] = key
      off_out[idx] = off
    end
  end
end
randkeygen_kern = compile(randkeygen_kern)

local terra merge_scatter( N : uint32,
                           bigoff_buf : &uint32, off_tmp : &uint32,
                           buf_tmp : &uint32,
                           buf_out : &uint32 -- holds the in-elem offset...
)
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var key       = buf_tmp[tid]
    var offset    = bigoff_buf[key] + off_tmp[tid]
    buf_out[offset] = key
  end
end
merge_scatter = compile(merge_scatter)

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
prefix_sum_1_kern = compile(prefix_sum_1_kern)
--]]

local merge_cache = {}
local function merge_test(n, N, rand, N_key, OUT_MAX)
  if not merge_cache.N_key then
    merge_cache.N_key     = N_key
    merge_cache.OUT_MAX   = OUT_MAX
    merge_cache.count_buf = terralib.cast(&uint32, GPU.malloc(4*N_key))
    merge_cache.offsets   = terralib.cast(&uint32, GPU.malloc(4*N_key))
    merge_cache.out_count = terralib.cast(&uint32, GPU.malloc(4))
    merge_cache.out_tmp   = terralib.cast(&uint32, GPU.malloc(4*OUT_MAX))
    merge_cache.off_tmp   = terralib.cast(&uint32, GPU.malloc(4*OUT_MAX))
    merge_cache.out_final = terralib.cast(&uint32, GPU.malloc(4*OUT_MAX))
  end
  assert(N_key == merge_cache.N_key, 'Nkey mismatch')
  assert(OUT_MAX == merge_cache.OUT_MAX, 'OUT_MAX mismatch')
  local count_buf     = merge_cache.count_buf
  local offsets       = merge_cache.offsets
  local out_count     = merge_cache.out_count
  local out_tmp       = merge_cache.out_tmp
  local off_tmp       = merge_cache.off_tmp
  local out_final     = merge_cache.out_final

  GPU.memzero(count_buf, 4*N_key)
  GPU.memzero(out_count, 4)
  randkeygen_kern(  n,  N, rand, N_key, count_buf, OUT_MAX,
                           out_count, out_tmp, off_tmp )

  GPU.prefix32(N, count_buf, offsets)

  merge_scatter( n, N, offsets, off_tmp, out_tmp, out_final )
end


------------------------------------------------------------------------------


local sort_cache32 = {}
local function sort_test32(n, N, rand)
  if not sort_cache32.N then
    sort_cache32.N          = N
    sort_cache32.key_in     = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache32.key_out    = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache32.val_in     = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache32.val_out    = terralib.cast(&uint32, GPU.malloc(4*N))
    write_rand(N, N, rand, terralib.cast(&uint32, sort_cache32.key_in) )
    write_rand(N, N, rand, sort_cache32.val_in)
  end
  assert(N == sort_cache32.N, 'N mismatch')
  local key_in            = sort_cache32.key_in
  local key_out           = sort_cache32.key_out
  local val_in            = sort_cache32.val_in
  local val_out           = sort_cache32.val_out

  local max_bit           = math.ceil(math.log(N)/math.log(2))
  GPU.sort32(N, max_bit, key_in, key_out, val_in, val_out)
end

local sort_cache64 = {}
local function sort_test64(n, N, rand)
  if not sort_cache64.N then
    sort_cache64.N          = N
    sort_cache64.key_in     = terralib.cast(&uint64, GPU.malloc(8*N))
    sort_cache64.key_out    = terralib.cast(&uint64, GPU.malloc(8*N))
    sort_cache64.val_in     = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache64.val_out    = terralib.cast(&uint32, GPU.malloc(4*N))
    write_rand(2*N, 2*N, rand, terralib.cast(&uint32, sort_cache64.key_in) )
    write_rand(N, N, rand, sort_cache64.val_in)
  end
  assert(N == sort_cache64.N, 'N mismatch')
  local key_in            = sort_cache64.key_in
  local key_out           = sort_cache64.key_out
  local val_in            = sort_cache64.val_in
  local val_out           = sort_cache64.val_out

  local max_bit           = math.ceil(math.log(N)/math.log(2))
  max_bit = max_bit + 32
  GPU.sort64(N, max_bit, key_in, key_out, val_in, val_out)
end

local terra init_idx( N : uint64, idxs : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    idxs[tid]     = tid
  end
end
init_idx = compile(init_idx)

local sort_cache = {}
local function sort_test_32_32(n, N, rand)
  if not sort_cache.N then
    sort_cache.N          = N
    sort_cache.key0_in    = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache.key0_out   = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache.key1_in    = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache.key1_out   = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache.segs       = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache.idxs       = terralib.cast(&uint32, GPU.malloc(4*N))
    write_rand(N, N, rand, sort_cache.key0_in)
    write_rand(N, N, rand, sort_cache.key1_in)
  end
  assert(N == sort_cache.N, 'N mismatch')
  local key0_in           = sort_cache.key0_in
  local key0_out          = sort_cache.key0_out
  local key1_in           = sort_cache.key1_in
  local key1_out          = sort_cache.key1_out
  local segs              = sort_cache.segs
  local idxs              = sort_cache.idxs

  --GPU.memcpy_on_gpu(key1, key1_ref, 4*N)
  init_idx(N, N, idxs)
  GPU.sort_32_32(N, N, key0_in, key0_out, key1_in, key1_out, segs, idxs)
end

local terra pack64( N : uint64, max_bit : int,
                    key0 : &uint32, key1 : &uint32, k64:&uint64 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var k0, k1    = [uint64]( key0[tid] ), [uint64]( key1[tid] )
    var k01       = (k0 << max_bit) or k1
    k64[tid]      = k01
  end
end
pack64 = compile(pack64)
local terra unpack64( N : uint64, max_bit : int,
                    key0 : &uint32, key1 : &uint32, k64:&uint64 )
  var tid         = [uint32](GPU.global_tid())
  if tid < N then
    var k01       = k64[tid]
    var mask      = (1 << max_bit) - 1
    var k0        = [uint32](k01 >> max_bit)
    var k1        = [uint32]((mask and k01))
    key0[tid]     = k0
    key1[tid]     = k1
  end
end
unpack64 = compile(unpack64)

local terra datashuffle( N   : uint64,  n_bytes : uint32,
                         idx : &uint32,
                         din : &uint32, dout : &uint32 )
  var tid         = [uint32](GPU.global_tid())
  var N_u32       = n_bytes/4
  if tid < N*N_u32 then
    var off       = tid % N_u32
    var write_i   = tid / N_u32
    var read_i    = idx[write_i]
    dout[N_u32 * write_i + off] = din[N_u32 * read_i + off]
  end
end
datashuffle = compile(datashuffle)

local sort_cache_T = {}
local function sort_test_64_T(n, N, rand)
  if not sort_cache_T.N then
    sort_cache_T.N          = N
    sort_cache_T.key0_in    = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache_T.key0_out   = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache_T.key1_in    = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache_T.key1_out   = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache_T.key64_in   = terralib.cast(&uint64, GPU.malloc(8*N))
    sort_cache_T.key64_out  = terralib.cast(&uint64, GPU.malloc(8*N))
    sort_cache_T.segs       = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache_T.idxs       = terralib.cast(&uint32, GPU.malloc(4*N))
    sort_cache_T.data_in    = terralib.cast(&uint32, GPU.malloc(32*N))
    sort_cache_T.data_out   = terralib.cast(&uint32, GPU.malloc(32*N))
    write_rand(N, N, rand, sort_cache_T.key0_in)
    write_rand(N, N, rand, sort_cache_T.key1_in)
  end
  assert(N == sort_cache_T.N, 'N mismatch')
  local key0_in           = sort_cache_T.key0_in
  local key0_out          = sort_cache_T.key0_out
  local key1_in           = sort_cache_T.key1_in
  local key1_out          = sort_cache_T.key1_out
  local key64_in          = sort_cache_T.key64_in
  local key64_out         = sort_cache_T.key64_out
  local segs              = sort_cache_T.segs
  local idxs              = sort_cache_T.idxs
  local data_in           = sort_cache_T.data_in
  local data_out          = sort_cache_T.data_out

  local max_bit           = math.ceil(math.log(N)/math.log(2))
  pack64(N, N, max_bit, key0_in, key1_in, key64_in)
  init_idx(N, N, key1_out)
  GPU.sort64(N, 2*max_bit, key64_in, key64_out, key1_out, idxs)
  unpack64(N, N, max_bit, key0_out, key1_out, key64_out)
  GPU.gen_segments(N, N, N, key0_out, segs)
  GPU.datashuffle(N, 32, idxs, data_in, data_out)
  -- some measurement notes
  -- 32 MB / 0.68 ms = 32 GB / 0.68 sec = 47 GB / sec
end




------------------------------------------------------------------------------




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

--
gpu_time('unsafe_gather', unsafe_gather, N, N, randbuf, buffer, buf_out)
gpu_time('write_rand', write_rand, N, N, randbuf, buffer)
gpu_time('reserve_test', reserve_test, N, N, randbuf, buffer,
                                          n_out, OUTSIZE, buf_out)
gpu_time('reserve_test', reserve_test, N, N, randbuf, buffer,
                                          n_out, OUTSIZE, buf_out)
gpu_time('sort_test32', sort_test32, N, N, randbuf)
gpu_time('sort_test64', sort_test64, N, N, randbuf)
gpu_time('sort_test_32_32', sort_test_32_32, N, N, randbuf)
gpu_time('sort_test_64_T',  sort_test_64_T,  N, N, randbuf)

print('n out... ', (terra()
  var g : uint32 = 0
  GPU.memcpy_from_gpu(&g, n_out, 4)
  return g
end)())

gpu_time('merge_test', merge_test, N, N, randbuf, N_key, OUTSIZE)
--]]

print('\nstart total timing\n')
GPU.sync()
local MEGA_START = terralib.currenttimeinseconds()

gpu_time('inc_buf', inc_buf, N, N, buffer)
gpu_time('inc_buf', inc_buf, N, N, buffer)
gpu_time('inc_buf x 100', run_n_times, 100, inc_buf, N, N, buffer)
gpu_time('inc_buf', inc_buf, N, N, buffer)

--
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

gpu_time('unsafe_gather', unsafe_gather, N, N, randbuf, buffer, buf_out)
gpu_time('unsafe_gather', unsafe_gather, N, N, randbuf, buffer, buf_out)
gpu_time('unsafe_gather x 100', run_n_times, 100,
                          unsafe_gather, N, N, randbuf, buffer, buf_out)
gpu_time('unsafe_gather', unsafe_gather, N, N, randbuf, buffer, buf_out)

gpu_time('unsafe_scatter', unsafe_scatter, N, N, randbuf, buffer, buf_out)
gpu_time('unsafe_scatter', unsafe_scatter, N, N, randbuf, buffer, buf_out)
gpu_time('unsafe_scatter x 100', run_n_times, 100,
                           unsafe_scatter, N, N, randbuf, buffer, buf_out)
gpu_time('unsafe_scatter', unsafe_scatter, N, N, randbuf, buffer, buf_out)

gpu_time('safe_scatter', safe_scatter, N, N, randbuf, buffer, buf_out)
gpu_time('safe_scatter', safe_scatter, N, N, randbuf, buffer, buf_out)
gpu_time('safe_scatter x 100', run_n_times, 100,
                         safe_scatter, N, N, randbuf, buffer, buf_out)
gpu_time('safe_scatter', safe_scatter, N, N, randbuf, buffer, buf_out)

--
gpu_time('merge_test', merge_test, N, N, randbuf, N_key, OUTSIZE)
gpu_time('merge_test', merge_test, N, N, randbuf, N_key, OUTSIZE)
gpu_time('merge_test x 100', run_n_times, 100,
                       merge_test, N, N, randbuf, N_key, OUTSIZE)
gpu_time('merge_test', merge_test, N, N, randbuf, N_key, OUTSIZE)

gpu_time('sort_test32', sort_test32, N, N, randbuf)
gpu_time('sort_test32', sort_test32, N, N, randbuf)
gpu_time('sort_test32 x 100', run_n_times, 100,
                        sort_test32, N, N, randbuf)
gpu_time('sort_test32', sort_test32, N, N, randbuf)

gpu_time('sort_test64', sort_test64, N, N, randbuf)
gpu_time('sort_test64', sort_test64, N, N, randbuf)
gpu_time('sort_test64 x 100', run_n_times, 100,
                        sort_test64, N, N, randbuf)
gpu_time('sort_test64', sort_test64, N, N, randbuf)

gpu_time('sort_test_32_32', sort_test_32_32, N, N, randbuf)
gpu_time('sort_test_32_32', sort_test_32_32, N, N, randbuf)
gpu_time('sort_test_32_32 x 100', run_n_times, 100,
                            sort_test_32_32, N, N, randbuf)
gpu_time('sort_test_32_32', sort_test_32_32, N, N, randbuf)

gpu_time('sort_test_64_T', sort_test_64_T, N, N, randbuf)
gpu_time('sort_test_64_T', sort_test_64_T, N, N, randbuf)
gpu_time('sort_test_64_T x 100', run_n_times, 100,
                           sort_test_64_T, N, N, randbuf)
gpu_time('sort_test_64_T', sort_test_64_T, N, N, randbuf)
--]]


--gpu_time('inc_buf x 100000', run_n_times, 100000, inc_buf, N, N, buffer)

local MEGA_STOP = terralib.currenttimeinseconds()

print('\ntotal timing  ', (MEGA_STOP - MEGA_START)*1e3, 'ms')

randbuf:destroy()
GPU.free(n_out)
GPU.free(globreg)
GPU.free(buf_out)
GPU.free(buffer)

