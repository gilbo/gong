#pragma once

#define USE_DOUBLE_PRECISION 0
#if USE_DOUBLE_PRECISION
typedef double Float;
#else
typedef float Float;
#endif

typedef uint32_t ObjID;

#define FCL_FILTER_NEIGHBORS 1
