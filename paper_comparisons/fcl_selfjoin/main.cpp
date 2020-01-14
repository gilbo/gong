#include <cstdio>
#include "collide.h"
int main() {
	nBodyBenchmark("rebuild_every_frame.csv", 0, 75, true);
	nBodyBenchmark("update_every_frame.csv", 0, 75, false);
    return 0;
}