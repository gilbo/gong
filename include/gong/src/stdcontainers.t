
local Exports     = {}
package.loaded['gong.src.stdcontainers'] = Exports


local C           = require 'gong.src.c'
local Util        = require 'gong.src.util'

-------------------------------------------------------------------------------

local DEFAULT_MINBYTES = 16
local function vector(T, MINALLOC)
  assert(terralib.types.istype(T))
  local TSZ               = terralib.sizeof(T)
  local DEFAULT_MINALLOC  = math.ceil(DEFAULT_MINBYTES/TSZ)
  MINALLOC                = MINALLOC or DEFAULT_MINALLOC

  local V                 = terralib.types.newstruct('vector_'..tostring(T))
  V.entries:insertall {
    { '_size',      uint32 },
    { '_alloced',   uint32 },
    { '_data',      &T     },
  }

  -- METHODS OF USE
  --    init
  --    destroy
  --    resize
  --    size
  --    access individual element
  --    ptr
  -- convenience methods
  --    insert
  --    remove    (these mirror lua list semantics)

  terra V:init()
    self._size      = 0
    self._alloced   = MINALLOC
    self._data      = [&T](C.malloc( MINALLOC*TSZ ))
  end
  terra V:destroy()
    C.free(self._data)
    self._data      = nil
    self._size      = 0
  end
  terra V:resize( newsize : uint32 )
    var oldsize     = self._size
    self._size      = newsize
    var oldalloc    = self._alloced
    if newsize > oldalloc then
      var newalloc  = oldalloc*2
      if newalloc < newsize then newalloc = newsize end
      self._alloced = newalloc
      self._data    = [&T](C.realloc( self._data, newalloc*TSZ ))
    elseif newsize <= oldalloc/4 and oldalloc >= 2*DEFAULT_MINALLOC then
      var newalloc  = oldalloc/2
      --if newalloc < DEFAULT_MINALLOC then newalloc = DEFAULT_MINALLOC end
      self._alloced = newalloc
      self._data    = [&T](C.realloc( self._data, newalloc*TSZ ))
    end
  end
  terra V:size()  return self._size end
  terra V:ptr()   return self._data end
  V.metamethods.__apply = macro(function(self, i)
    return `self._data[i]
  end)

  return V
end
vector = Util.memoize(vector)

Exports.vector = vector



