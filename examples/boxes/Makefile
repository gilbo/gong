

TERRA_FILES = boxes.t main_terra.t PGS_terra.t prelude.t

TEST = round_tower0

run: $(TERRA_FILES)
	time ../../bin/gong main_terra.t -test_case=$(TEST) -gpu=true > scan_gpu_cout.txt
	time ../../bin/gong main_terra.t -test_case=$(TEST) -traversal=scan_scan  > scan_cout.txt
	time ../../bin/gong main_terra.t -test_case=$(TEST) -traversal=bvh_bvh    > bvh_cout.txt


temp:
	time ../../bin/gong main_terra.t \
	-test_case=round_tower0 -traversal=bvh_bvh -seed=3 -gpu=true \
	> round_tower_GPU_bvh.txt


clean: