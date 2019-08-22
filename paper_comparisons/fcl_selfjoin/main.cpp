#include <cstdio>
#include "collide.h"
int main() {
    /*
    std::string filename = "../data/funnel.plys/253.ply";
    std::vector<int> componentStarts = { 0, 7377 };
    */
    /*
    std::vector<int> componentStarts = { 0, 46216 };
    for (int i = 0; i < 94; ++i) {
        char filename[100];
        printf("clothball %d\n", i);
        sprintf(filename, "../data/cloth_ball.plys/cloth_ball%d.ply", i);
        runCollisionTest(filename, componentStarts, { 0.0f, 0.0f, 0.0f });
    }*/
    /*
    std::vector<int> componentStarts = { 0, 7377 };
    for (int i = 1; i < 501; ++i) {
        char filename[100];
        printf("funnel %d\n", i);
        sprintf(filename, "../data/funnel.plys/%03d.ply", i);
        runCollisionTest(filename, componentStarts, { 0.0f, 0.0f, 0.0f });
    }
    */
    
    for (int i = 0; i < 1; ++i) {
        char filename[100];
        printf("balls16_ %d\n", i);
        sprintf(filename, "../data/balls16_.plys/balls16_%d.ply", i);
        runCollisionTestOnConnectedComponents(filename);
    }
    

    return 0;
}