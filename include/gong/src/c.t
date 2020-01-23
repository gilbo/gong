
local Exports = {}
package.loaded['gong.src.c'] = Exports



-------------------------------------------------------------------------------
-- Intel x87 floating point behavior control to ensure determinism
-------------------------------------------------------------------------------

--local terra warpballot_b32() : uint32
--  return terralib.asm(terralib.types.uint32,
--    "vote.ballot.b32 $0, 0xFFFFFFFF;","=r",false)
--end
--#define _FPU_GETCW(cw) __asm__ ("fnstcw %0" : "=m" (*&cw))
--#define _FPU_SETCW(cw) __asm__ ("fldcw %0" : : "m" (*&cw))


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
local sys_time
local perf_time
local ffi = require("ffi")
if ffi.os == "Windows" then
  sys_time = [[
  #include <Windows.h>
  double get_wall_time() {
    unsigned __int64 pf;
    QueryPerformanceFrequency((LARGE_INTEGER *)&pf);
    double freq_ = 1.0 / (double)pf;

    unsigned __int64 val;
    QueryPerformanceCounter((LARGE_INTEGER *)&val);
    return (val)* freq_;
  }
  ]]
  perf_time =   [[
  void initialize_performance_timer() {
    return;
  }
  double get_perf_time_in_seconds() {
    unsigned __int64 pf;
    QueryPerformanceFrequency((LARGE_INTEGER *)&pf);
    double freq_ = 1.0 / (double)pf;

    unsigned __int64 val;
    QueryPerformanceCounter((LARGE_INTEGER *)&val);
    return (val)* freq_;
  }
  ]]
else
  -- system timer
  sys_time = [[
  #include <sys/time.h>
  double get_wall_time(){
    struct timeval time;
    if (gettimeofday(&time,NULL)){
      return 0;
    }
    return (double)time.tv_sec + (double)time.tv_usec * .000001;
  }
  ]]

  -- performance timer
  perf_time = [[
  #include <time.h>
  time_t    perf_init_time = 0;
  void initialize_performance_timer() {
    if (perf_init_time == 0) {
      struct timespec ts;
      clock_gettime( CLOCK_REALTIME, &ts );
      perf_init_time = ts.tv_sec;
    }
  }
  double get_perf_time_in_seconds() {
    struct timespec ts;
    clock_gettime( CLOCK_REALTIME, &ts );
    // note that the difference here keeps the magnitude of the
    // timestamp small enough that nanoseconds will not be rounded off
    // for the first 1e6 seconds of program execution, which is about
    // 11.5 days
    return (double)(ts.tv_sec - perf_init_time) +
           (double)(ts.tv_nsec) * 1e-9;
  }
  ]]
end

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

// These routines seem to be needed on some versions of Terra,
// but not for all versions.  Go figure.

FILE * __gong_get__stdout() { return stdout; }
FILE * __gong_get__stdin()  { return stdin; }
FILE * __gong_get__stderr() { return stderr; }

]]..
sys_time..
perf_time
)

-- mass export the c code
for k,v in pairs(C) do 
  Exports[k] = v 
end
if ffi.os == "Windows" then -- Workaround since snprintf doesn't properly export.
  Exports.snprintf = Exports._snprintf
end

Exports.__gong_get__stderr  = nil
Exports.__gong_get__stdin   = nil
Exports.__gong_get__stdout  = nil

if Exports.stdout then
  local terra stdout() return C.stdout end
  local terra stdin()  return C.stdin  end
  local terra stderr() return C.stderr end
  Exports.stdout  = stdout
  Exports.stdin   = stdin
  Exports.stderr  = stderr
else
  local terra stdout() return C.__gong_get__stdout() end
  local terra stdin()  return C.__gong_get__stdin()  end
  local terra stderr() return C.__gong_get__stderr() end
  Exports.stdout  = stdout
  Exports.stdin   = stdin
  Exports.stderr  = stderr
end


-------------------------------------------------------------------------------
-- Standard Terra macros to recover some C functionality
-------------------------------------------------------------------------------

-- provide a modified assert statement that will
-- work correctly inside of Terra code
local oldassert = assert
local assert = macro(function(test,errstr,...)
  local optargs     = terralib.newlist{...}
  local filename    = test.filename or 'unknown_file'
  local linenumber  = test.linenumber or -1
  errstr = errstr and errstr:asvalue()
  errstr = type(errstr) == 'string' and errstr or nil
  local err = tostring(filename)..":"..tostring(linenumber)..
                 ": assertion failed!\n"
  if errstr then err = err .. errstr .. "\n" end
  return quote
    if not test then
      C.fflush(Exports.stdout())
      C.printf("stderr: %p\n", Exports.stderr())
      C.fprintf(Exports.stderr(), err, [optargs])
      terralib.traceback(nil)
      C.abort()
    end
  end
end,oldassert)

Exports.assert = assert


