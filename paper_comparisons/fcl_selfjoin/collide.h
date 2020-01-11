#pragma once
#include <string>
#include <vector>
#include "config.h"
#include <unordered_map>
typedef std::unordered_map<size_t, uint64_t> GeometryIDMap;
#include "fcl/common/types.h"
#include "fcl/math/triangle.h"
using fcl::Vector3;
using fcl::Triangle;

struct vec3f { Float x, y, z; };


static bool approxEqual(Float x0, Float x1, Float epsilon = 1e-5) {
	return std::abs(x0 - x1) < epsilon;
}

static bool approxEqual(vec3f v0, vec3f v1, Float epsilon = 1e-5) {
	return approxEqual(v0.x, v1.x) && approxEqual(v0.y, v1.y) && approxEqual(v0.z, v1.z);
}

struct ComparisonContact {
	uint64_t obj0 = -1;
	uint64_t obj1 = -1;
	uint32_t tri0;
	uint32_t tri1;
	vec3f pos;
	Float depth;
	vec3f nrml;
	ComparisonContact() {}
	ComparisonContact(uint64_t o0, uint64_t o1, uint32_t t0, uint32_t t1, vec3f p, Float d, vec3f n) :
		obj0(o0), obj1(o1), tri0(t0), tri1(t1), pos(p), depth(d), nrml(n) {
		canonicalize();
	}
	bool operator==(const ComparisonContact& c) const {
		ComparisonContact c0 = *this;
		ComparisonContact c1 = c;
		c0.canonicalize();
		c1.canonicalize();
		return
			c0.tri0 == c1.tri0 &&
			c0.tri1 == c1.tri1 &&
			c0.obj0 == c1.obj0 &&
			c0.obj1 == c1.obj1 &&
			//approxEqual(c0.nrml, c1.nrml) &&
			approxEqual(c0.pos, c1.pos);
	}
	void canonicalize() {
		if (obj0 > obj1) {
			std::swap(obj0, obj1);
			std::swap(tri0, tri1);
			nrml = { -nrml.x, -nrml.y, -nrml.z };
		}
	}
};

class ComparisonContactHashFunction {
public:
	// Only compare tri and obj IDs for hash. Not a good hash function, but works in a pinch
	// And is better than doing a full linear scan...
	size_t operator()(const ComparisonContact& t) const
	{
		return ((size_t)t.tri0*t.tri0*t.tri0 + (t.obj0*t.obj0)) + ((size_t)t.tri1*t.tri1*t.tri1) + (t.obj1*t.obj1);
	}
};


void runCollisionTestOnConnectedComponents(std::string filename);

void runFCLCollisionOnComponents(
	const std::vector<std::vector<Vector3<Float>>>& splitVerts, 
	const std::vector<std::vector<Triangle>>& splitTris, 
	std::vector<ComparisonContact>& fclContacts);

