

local Exports = {}
package.loaded['gong.src.cub_wrap'] = Exports

local PARAMETER     = (require 'gong.src.params').get_param

-- only run this file when CUDA is enabled
if not PARAMETER('GPU_ENABLED') then return end

local newlist     = terralib.newlist

-- The strategy for this file:
--  1)  Locate CUDA and other resources on the filesystem.
--      We will check to see if they do not exist or need to be updated
--  2)  Specify the wrapper file in all cases, regardless of whether
--      files need to be updated
--  3)  If needed, recompile the .so file using nvcc
--  4)  Link in the .so code, which we are sure is fresh compared to
--      the last update time on this file itself.
--
-- This strategy has the property that all specification of the wrapper
-- is contained HERE and not on other files on disk.  Those other files
-- simply exist to be orchestrated from here.


-------------------------------------------------------------------------------
--[[                          Paths - Locate CUDA                          ]]--
-------------------------------------------------------------------------------


local Pathname  = (require "gong.tools.pathname").Pathname
--local scriptdir = (Pathname.scriptdir()):abspath():cleanpath()

local runtime_path    = Pathname.new( terralib.cudalibpaths.runtime )
local cudalib_dir     = runtime_path:dirpath()
local cuda_dir        = (cudalib_dir .. '..'):cleanpath()

local lib_dir         = Pathname.gong_root:abspath() .. 'gong/libs'

local cub_dir         = lib_dir..'cub-1.8.0'
local cub_wrap_obj    = lib_dir..'libcub_wrap.so'
local cub_wrap_src    = lib_dir..'cub_wrap.cu'
local cub_wrap_header = lib_dir..'cub_wrap.h'
local this_file       = Pathname.scriptdir():abspath():cleanpath()
                        .. 'cub_wrap.t'

local POSIX = terralib.includecstring [[
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

struct statwrap { struct stat s; };
]]

local obj_fresh = false
if cub_wrap_obj:exists() then
  local S = terralib.new(POSIX.statwrap)
  assert(0 == POSIX.stat(tostring(cub_wrap_obj), S.s),
         'INTERNAL: posix call to stat() failed')
  local obj_mod_sec   = S.s.st_mtim.tv_sec
  assert(0 == POSIX.stat(tostring(this_file), S.s),
         'INTERNAL: posix call to stat() failed')
  local this_mod_sec  = S.s.st_mtim.tv_sec
  if obj_mod_sec > this_mod_sec then
    obj_fresh = true
  end
end


-------------------------------------------------------------------------------
--[[                         Library Specification                         ]]--
-------------------------------------------------------------------------------


local names   = newlist()
local funcs   = newlist()
local sigs    = newlist()
local function addfunc(name,sig,body)
  names:insert(name)
  sigs:insert(sig..';')
  funcs:insert(sig..body)
end

addfunc('cub_wrap_get_sort32_bytes',[[
int cub_wrap_get_sort32_bytes(int N, size_t *out)]],
[[{
  size_t n_bytes  = 0;
  uint32_t *nil   = NULL;
  int retcode     = cub::DeviceRadixSort::SortPairs(
                      nil, n_bytes, nil, nil, nil, nil, N
                    );
  *out = n_bytes;
  return retcode;
}]])

addfunc('cub_wrap_get_sort64_bytes',[[
int cub_wrap_get_sort64_bytes(int N, size_t *out)]],
[[{
    size_t n_bytes  = 0;
    uint32_t *nil32 = NULL;
    uint64_t *nil64 = NULL;
    int retcode     = cub::DeviceRadixSort::SortPairs(
                        NULL, n_bytes, nil64, nil64, nil32, nil32, N
                      );
    *out = n_bytes;
    return retcode;
}]])

addfunc('cub_wrap_do_sort32',[[
int cub_wrap_do_sort32( int N, int maxBits, void *tmp, uint64_t n_tmp,
                        uint32_t *key_in, uint32_t *key_out,
                        uint32_t *val_in, uint32_t *val_out )]],
[[{
    return cub::DeviceRadixSort::SortPairs( tmp, n_tmp,
                                            key_in, key_out,
                                            val_in, val_out,
                                            N, 0, maxBits);
}]])

addfunc('cub_wrap_do_sort64',[[
int cub_wrap_do_sort64( int N, int maxBits, void *tmp, uint64_t n_tmp,
                        uint64_t *key_in, uint64_t *key_out,
                        uint32_t *val_in, uint32_t *val_out )]],
[[{
    return cub::DeviceRadixSort::SortPairs( tmp, n_tmp,
                                            key_in, key_out,
                                            val_in, val_out,
                                            N, 0, maxBits);
}]])

addfunc('cub_wrap_get_prefix32_bytes',[[
int cub_wrap_get_prefix32_bytes(int N, size_t *out)]],
[[{
  size_t n_bytes  = 0;
  uint32_t *nil   = NULL;
  int retcode     = cub::DeviceScan::ExclusiveSum(
                      nil, n_bytes, nil, nil, N
                    );
  *out = n_bytes;
  return retcode;
}]])

addfunc('cub_wrap_do_prefix32',[[
int cub_wrap_do_prefix32( int N, void *tmp, uint64_t n_tmp,
                        uint32_t *buf_in, uint32_t *buf_out )]],
[[{
    return cub::DeviceScan::ExclusiveSum( tmp, n_tmp,
                                          buf_in, buf_out, N);
}]])



addfunc('cub_wrap_get_seg_sort32_bytes',[[
int cub_wrap_get_seg_sort32_bytes( int N, int Nseg, size_t *out)]],
[[{
    size_t n_bytes  = 0;
    uint32_t *nil   = NULL;
    int retcode     = cub::DeviceSegmentedRadixSort::SortPairs(
                        nil, n_bytes, nil, nil, nil, nil, N, 
                        Nseg, nil, nil
                      );
    *out = n_bytes;
    return retcode;
}]])
addfunc('cub_wrap_do_seg_sort32',[[
int cub_wrap_do_seg_sort32( int N, int maxBits, void *tmp, uint64_t n_tmp,
                            int Nseg, uint32_t *seg_off,
                            uint32_t *key_in, uint32_t *key_out,
                            uint32_t *val_in, uint32_t *val_out )]],
[[{
    return cub::DeviceSegmentedRadixSort::SortPairs(
      tmp, n_tmp,
      key_in, key_out, val_in, val_out,
      N,
      Nseg, seg_off, seg_off+N,
      0, maxBits);
}]])


-------------------------------------------------------------------------------
--[[                        .so Library Generation                         ]]--
-------------------------------------------------------------------------------


-- Take action to update the object file if necessary
if not obj_fresh then

local h_file_text = [[
#ifndef __GONG_CUB_WRAP__H_
#define __GONG_CUB_WRAP__H_

#include <stdint.h>
#include <stdlib.h>

]]..sigs:concat('\n')..[[


#endif /* __GONG_CUB_WRAP__H_ */
]]
local hfout = io.open(tostring(cub_wrap_header), 'w')
hfout:write(h_file_text)
hfout:close()

local cu_file_text = [[
#include <cub/device/device_radix_sort.cuh>
#include <cub/device/device_segmented_radix_sort.cuh>
#include <cub/device/device_scan.cuh>

extern "C" {
]]..sigs:concat('\n')
..[[

}
]]..
funcs:concat('\n')..[[


]]
local cufout = io.open(tostring(cub_wrap_src), "w")
cufout:write(cu_file_text)
cufout:close()

-- Determine the local version to feed to NVCC
local cuda_success, cuda_version  =
  pcall(function() return cudalib.localversion() end)
if not cuda_success then
  error("could not determine local CUDA version")
else
local sm = cuda_version

-- Command to compile the shared-object from the .cu file
print('generating cub_wrap.so wrapper around needed CUDA UnBound code\n'..
      '  in order to expose C calls to Gong...\n')
local compile_so = 'nvcc -I '..tostring(cub_dir)..'\\\n'..
                   '     -gencode=arch=compute_'..tostring(sm)..
                                     ',code=sm_'..tostring(sm)..' \\\n'..
                   '     --compiler-options \'-fPIC\' --shared \\\n'..
                   '     -O3 \\\n'..
                   '     '..tostring(cub_wrap_src)..
                            ' -o '..tostring(cub_wrap_obj)
print(compile_so)
local SO_success = os.execute(compile_so)
os.execute('rm '..tostring(cub_wrap_src))
if SO_success == 0 then
  print('\nlibcub_wrap.so generated.')
else
  error("libcub_wrap.so compilation FAILED: "..tostring(SO_success))
end -- SO_success
end -- cuda_success
end -- obj_fresh

-------------------------------------------------------------------------------
--[[                            Library Linking                            ]]--
-------------------------------------------------------------------------------

-- link library?
terralib.linklibrary(tostring(cub_wrap_obj))

local CUB       = terralib.includecstring([[
#include "cub_wrap.h"]], {"-I", tostring(lib_dir)})

--local CUB = terralib.includecstring( '#include ""'
--  '#include <stdint.h>\n'..
--  '#include <stdlib.h>\n'..
--  --'#define bool uint8_t\n'..
--  sigs:concat('\n')
--)


for _,k in ipairs(names) do Exports[k] = CUB[k] end





