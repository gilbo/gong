#pragma once
#include <string>
#include <vector>

#define USE_DOUBLE_PRECISION 0
#if USE_DOUBLE_PRECISION
    typedef double Float;
#else
    typedef float Float;
#endif
struct vec3f { Float x, y, z; };
void runCollisionTest(std::string filename, const std::vector<int>& componentStarts, vec3f gravStep);
void runCollisionTestOnConnectedComponents(std::string filename);