
local Exports = {}
package.loaded['gong.src.c'] = Exports


local C = terralib.includecstring [[
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

FILE * __get_seam_c__stdout() { return stdout; }
FILE * __get_seam_c__stdin()  { return stdin; }
FILE * __get_seam_c__stderr() { return stderr; }
]]

-- mass export the c code
for k,v in pairs(C) do Exports[k] = v end

-- expose the std file handles
local stdout = C.__get_seam_c__stdout()
local stdin  = C.__get_seam_c__stdin()
local stderr = C.__get_seam_c__stderr()
Exports.stdout = stdout
Exports.stdin  = stdin
Exports.stderr = stderr

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
