import 'gong'
local G = gong.stdlib

local Exports = {}
package.loaded["prelude"] = Exports

------------------------------------------------------------------------------

Exports.num = G.double

local C = terralib.includecstring [[
  #include "float.h"
  float _GET_FLT_EPSILON() { return FLT_EPSILON; }
  double _GET_DBL_EPSILON() { return DBL_EPSILON; }
]]

Exports.FLT_EPSILON = C._GET_FLT_EPSILON()
Exports.DBL_EPSILON = C._GET_DBL_EPSILON()
