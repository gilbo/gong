#!/bin/bash
#for i in 0 1 2 3 4 5 6 7
for i in 1 5
do
   make clean;make -e BVHNodeType=$i;./main
done
