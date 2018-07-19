import 'gong'
local test  = require 'tests.test'

local G     = gong.stdlib

------------------------------------------------------------------------------


local ValS = G.record{ {'s',G.double},{'t',G.int32} }

local A = G.NewTable('A')
local B = G.NewTable('B')
local C = G.NewTable('C')
A:NewField('id', G.int32)
A:NewField('val', ValS)
B:NewField('id', G.int32)
B:NewField('val', G.double)
C:NewField('a', A)
C:NewField('b', B)
local gong join aboff( a : A, b : B )
  where a.id+1 == b.id
do
  emit { a=a, b=b } in C
end


G.CompileLibrary {
  prefix          = 'simple',
  tables          = {},
  joins           = {aboff},
  c_obj_file      = 'simple_gong.o',
  cpp_header_file = 'simple_gong.h',
}


do -- write out driver program
local F = io.open("simple_prog.cpp","w")
F:write([[
#include "simple_gong.h"
#include <iostream>

#define CHECK_ERR() { \
  if(store.geterror()) { \
    std::cerr << store.geterror() << std::endl; \
    return 1; \
} }

#define ERR(msg) { \
 \
  std::cerr << "ERROR: " << msg << std::endl; \
  return 1; \
}

int main() {
  Store store  = Store::NewStore();
  // load
  CHECK_ERR();

  store.A().beginload(4);
    store.A().loadrow(1, (__s_double__t_int32__){4.2, 9});
    store.A().loadrow(2, (__s_double__t_int32__){2.1, 74});
    store.A().loadrow(3, (__s_double__t_int32__){3.9, 91});
    store.A().loadrow(4, (__s_double__t_int32__){9.6, 34});
  store.A().endload();
  CHECK_ERR();

  store.B().beginload(4);
    store.B().loadrow(1, 41.2);
    store.B().loadrow(2, 53.6);
    store.B().loadrow(3, 73.9);
    store.B().loadrow(4, 23.4);
  store.B().endload();
  CHECK_ERR();

  // Do the Join
  store.aboff();
  CHECK_ERR();

  // Check the result
  int Csize   = store.C().size();
  std::cout << "Size of Join Result: " << Csize << std::endl;
  if(Csize != 3) { ERR("bad join result size"); }

  for(int k=0; k<3; k++) {
    int a = store.C().a().read(k);
    int b = store.C().b().read(k);
    std::cout << "row " << k << ": (" << a << "," << b << ")" << std::endl;
  }

  // cleanup
  store.destroy();
  return 0;
}
]])
F:close()
end

local function cleanup()
  local function safe_rm(f)
    os.execute("[ -f "..f.." ] && rm "..f)
  end
  safe_rm("simple_prog")
  safe_rm("simple_gong.o")
  safe_rm("simple_gong.h")
  safe_rm("simple_prog.cpp")
end

local compile_code =
  os.execute("c++ -std=c++11 -o simple_prog simple_prog.cpp simple_gong.o")
if compile_code ~= 0 then
  --cleanup()
  error("Failed to Compile")
end
local exec_code = os.execute("./simple_prog")
if exec_code ~= 0 then
  cleanup()
  error("Error during test execution")
end

cleanup()






