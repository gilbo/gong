
local vdb = {}
package.loaded["seam.tools.vdb"] = vdb

local Pathname  = (require "seam.tools.pathname").Pathname
local libdir    = Pathname.scriptdir()
local VDB_C     = terralib.includecstring([[
#include <vdb.h>]], {"-I", tostring(libdir)})


vdb.color     = VDB_C.vdb_color
vdb.normal    = VDB_C.vdb_normal
vdb.point     = VDB_C.vdb_point
vdb.line      = VDB_C.vdb_line
vdb.triangle  = VDB_C.vdb_triangle
vdb.vbegin    = VDB_C.vdb_begin
vdb.vend      = VDB_C.vdb_end
vdb.flush     = VDB_C.vdb_flush
vdb.frame     = VDB_C.vdb_frame

