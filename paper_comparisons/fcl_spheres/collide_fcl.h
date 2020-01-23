#pragma once
#include <string>
#include <vector>
#include <fstream>
#include "config.h"
#include <unordered_map>
#include "timer.h"
#include <iostream>

#define RADIUS 1.0f
#include "fcl/common/types.h"
using fcl::Vector3;

struct FCLState;

FCLState* initializeFCL(const std::vector<Vector3<Float>>& centers, int managerIndex=4);
double updateFCLPositions(FCLState* state, const std::vector<Vector3<Float>>& centers);
double buildFCLAccelerationStructure(FCLState* state);
double fclCollision(FCLState* state, int& collisionCount);
void fclCleanup(FCLState* state);

