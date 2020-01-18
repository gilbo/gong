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

	for (int i = 0; i < managerNames.size(); ++i) {
		std::string suff = "_"+managerNames+"_"+suffix+".csv";
		nBodyBenchmark("rebuild_every_frame"+suff, 0, 75, true,i);
		nBodyBenchmark("update_every_frame"+suff, 0, 75, false,i);
	}
    return 0;
}