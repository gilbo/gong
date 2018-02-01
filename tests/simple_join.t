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
  prefix        = 'simple',
  tables        = {},
  joins         = {aboff},
  c_obj_file    = 'simple_gong.o',
  c_header_file = 'simple_gong.h',
}


do -- write out driver program
local F = io.open("simple_prog.c","w")
F:write([[
#include "simple_gong.h"
#include <stdio.h>

#define CHECK_ERR() { \
  if(simple_GetError(store)) { \
    printf("%s\n",simple_GetError(store)); \
    return 1; \
} }

#define ERR(msg) { \
  printf("ERROR: %s\n", msg); \
  return 1; \
}

int main() {
  simple_Store store  = simple_NewStore();
  // load
  CHECK_ERR();

  simple_BeginLoad_A(store, 4);
    simple_LoadRow_A(store, 1, (__s_double__t_int32__){4.2, 9});
    simple_LoadRow_A(store, 2, (__s_double__t_int32__){2.1, 74});
    simple_LoadRow_A(store, 3, (__s_double__t_int32__){3.9, 91});
    simple_LoadRow_A(store, 4, (__s_double__t_int32__){9.6, 34});
  simple_EndLoad_A(store);
  CHECK_ERR();

  simple_BeginLoad_B(store, 4);
    simple_LoadRow_B(store, 1, 41.2);
    simple_LoadRow_B(store, 2, 53.6);
    simple_LoadRow_B(store, 3, 73.9);
    simple_LoadRow_B(store, 4, 23.4);
  simple_EndLoad_B(store);
  CHECK_ERR();

  // Do the Join
  simple_aboff(store);
  CHECK_ERR();

  // Check the result
  int Csize   = simple_GetSize_C(store);
  printf("Size of Join Result: %d\n", Csize);
  if(Csize != 3) { ERR("bad join result size"); }

  for(int k=0; k<3; k++) {
    int a = simple_Read_C_a( store, k );
    int b = simple_Read_C_b( store, k );
    printf("row %d: (%d,%d)\n", k, a, b);
  }

  // cleanup
  simple_DestroyStore(store);
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
  safe_rm("simple_prog.c")
end

local compile_code =
  os.execute("cc -o simple_prog simple_prog.c simple_gong.o")
if compile_code ~= 0 then
  cleanup()
  error("Failed to Compile")
end
local exec_code = os.execute("./simple_prog")
if exec_code ~= 0 then
  cleanup()
  error("Error during test execution")
end

cleanup()






