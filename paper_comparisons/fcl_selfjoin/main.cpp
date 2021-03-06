#include <cstdio>
#include "collide.h"

int main() {

	std::vector<std::string> bvhNodeNames = {
		"OBBRSS",
		"AABB",
		"OBB",
		"RSS",
		"kIOS",
		"KDOP16",
		"KDOP18",
		"KDOP24"
	};

	std::string suffix = bvhNodeNames[BVHNodeType];

	std::vector<std::string> managerNames = {
  		"Naive",
  		"SSaP",
  		"SaP",
  		"IntervalTree",
  		"DynamicAABBTree",
  		"DynamicAABBTree_Array"
	};

	bool flattenedFCL = true;
	if (flattenedFCL) {
		std::string suff = "_"+suffix+".csv";
		nBodyBenchmark("rebuild_every_frame"+suff, 0, 75, true,0,flattenedFCL);
		nBodyBenchmark("update_every_frame"+suff, 0, 75, false,0,flattenedFCL);
	} else {
		for (int i = 0; i < 1; ++i) {
			std::string suff = "_"+managerNames[i]+"_"+suffix+".csv";
			nBodyBenchmark("rebuild_every_frame"+suff, 0, 75, true,i,flattenedFCL);
			nBodyBenchmark("update_every_frame"+suff, 0, 75, false,i,flattenedFCL);
		}
	}
		
    return 0;
}
