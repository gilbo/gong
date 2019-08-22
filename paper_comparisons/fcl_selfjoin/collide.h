#pragma once
#include <string>
#include <vector>
struct vec3 { float x, y, z; };
void runCollisionTest(std::string filename, const std::vector<int>& componentStarts, vec3 gravStep);
void runCollisionTestOnConnectedComponents(std::string filename);