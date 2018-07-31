
local Exports = {}
package.loaded['gong.src.c'] = Exports


-------------------------------------------------------------------------------
-- GPU optional...
-------------------------------------------------------------------------------

local PARAMETER     = (require 'gong.src.params').get_param

local cuda_include = ""
if PARAMETER('GPU_ENABLED') then
  cuda_include = [[
  #include "cuda_runtime.h"
  #include "driver_types.h"
  ]]
end


-------------------------------------------------------------------------------
-- Additional more complex C functionality
-------------------------------------------------------------------------------

-- system timer
local sys_time = [[
#include <sys/time.h>
double get_wall_time(){
  struct timeval time;
  if (gettimeofday(&time,NULL)){
    return 0;
  }
  return (double)time.tv_sec + (double)time.tv_usec * .000001;
}
]]


-------------------------------------------------------------------------------
-- Main Header load
-------------------------------------------------------------------------------

local C = terralib.includecstring(
cuda_include..
[[
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

//#include <float.h>
//#include <limits.h>
#include <math.h>
//#include <time.h>

FILE * __get_seam_c__stdout() { return stdout; }
FILE * __get_seam_c__stdin()  { return stdin; }
FILE * __get_seam_c__stderr() { return stderr; }
]]..
sys_time
)

-- mass export the c code
for k,v in pairs(C) do Exports[k] = v end

-- expose the std file handles
local stdout = C.__get_seam_c__stdout()
local stdin  = C.__get_seam_c__stdin()
local stderr = C.__get_seam_c__stderr()
Exports.stdout = stdout
Exports.stdin  = stdin
Exports.stderr = stderr


-------------------------------------------------------------------------------
-- Standard Terra macros to recover some C functionality
-------------------------------------------------------------------------------

-- provide a modified assert statement that will
-- work correctly inside of Terra code
local oldassert = assert
local assert = macro(function(test,errstr,...)
  local filename    = test.filename or 'unknown_file'
  local linenumber  = test.linenumber or -1
  errstr = errstr and errstr:asvalue()
  errstr = type(errstr) == 'string' and errstr or nil
  local err = tostring(filename)..":"..tostring(linenumber)..
                 ": assertion failed!\n"
  if errstr then err = err .. errstr .. "\n" end
  return quote
    if not test then
      C.fprintf(stderr,err,[{...}])
      terralib.traceback(nil)
      C.abort()
    end
  end
end,oldassert)

Exports.assert = assert


