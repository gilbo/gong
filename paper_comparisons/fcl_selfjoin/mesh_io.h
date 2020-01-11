#pragma once
#include <chrono>

#include "config.h"

#include "fcl/common/types.h"
#include "fcl/math/triangle.h"
using fcl::Vector3;
using fcl::Triangle;
void readPly(const std::string & filepath, std::vector<Vector3<Float>>& verts, std::vector<Triangle>& tris);
void writePly(const std::string & filename, const std::vector<Vector3<Float>>& verts, const std::vector<Triangle>& tris);

void splitIntoComponents(
	const std::vector<Vector3<Float>>& verts, 
	const std::vector<Triangle>& tris,
	const std::vector<int>& vertexStartLocations,
	std::vector<std::vector<Vector3<Float>>>& outVerts, 
	std::vector<std::vector<Triangle>>& outTris);

void splitIntoConnectedComponents(
	const std::vector<Vector3<Float>>& verts, 
	const std::vector<Triangle>& tris,
	std::vector<std::vector<Vector3<Float>>>& outVerts, 
	std::vector<std::vector<Triangle>>& outTris, 
	std::vector<uint64_t>& outObjIDs, 
	std::vector<int>& startTriIDs);