#pragma once
#include <string>
#include <vector>
struct vec3f { float x, y, z; };
void runCollisionTest(std::string filename, const std::vector<int>& componentStarts, vec3f gravStep);
void runCollisionTestOnConnectedComponents(std::string filename);