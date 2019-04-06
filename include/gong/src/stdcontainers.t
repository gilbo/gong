
local Exports     = {}
package.loaded['gong.src.stdcontainers'] = Exports


local C           = require 'gong.src.c'
local Util        = require 'gong.src.util'

-------------------------------------------------------------------------------

local function sort(elemType)
  local eSZ       = terralib.sizeof(elemType)
  -- insertion sort
  local terra sort( xs : &elemType, N : uint32 )
    for i=0,N do
      var min     = xs[i]
      var min_idx = i
      for j=i+1,N do
        if xs[j] < min then
          min = xs[j]
          min_idx = j
        end
      end
      if min_idx ~= i then
        xs[i], xs[min_idx] = xs[min_idx], xs[i]
      end
    end
  end
  return sort
end
sort = Util.memoize(sort)


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

  terra V:push_back( x : T )
    var N = self:size()
    self:resize(N+1)
    self(N) = x
  end

  return V
end
vector = Util.memoize(vector)

-------------------------------------------------------------------------------

local function stack(T, MINALLOC)
  assert(terralib.types.istype(T))

  local S                 = terralib.types.newstruct('stack_'..tostring(T))
  S.entries:insertall {
    { '_vec_impl',  vector(T,MINALLOC) }
  }

  -- METHODS OF USE
  --    init
  --    destroy
  --    is_empty
  --    size
  --    top_ptr
  --    push
  --    pop

  terra S:init()
    self._vec_impl:init()
  end
  terra S:destroy()
    self._vec_impl:destroy()
  end
  terra S:size()      return self._vec_impl:size() end
  terra S:is_empty()  return self:size() == 0 end
  terra S:top_ptr() : &T
    return self._vec_impl:ptr() + self._vec_impl:size() - 1
  end
  terra S:push( val : T )
    var idx     = self._vec_impl:size()
    self._vec_impl:resize(idx+1)
    self._vec_impl(idx) = val
  end
  terra S:pop() : T
    var idx     = self._vec_impl:size() - 1
    var val     = self._vec_impl(idx)
    self._vec_impl:resize(idx)
    return val
  end

  return S
end
stack = Util.memoize(stack)

-------------------------------------------------------------------------------


Exports.sort    = sort
Exports.vector  = vector
Exports.stack   = stack



