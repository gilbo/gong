

TERRA_FILES = boxes.t main_terra.t PGS_terra.t prelude.t

TEST = round_tower

run: $(TERRA_FILES)
	time ../../bin/gong main_terra.t -test_case=$(TEST) -gpu=true > scan_gpu_cout.txt
	time ../../bin/gong main_terra.t -test_case=$(TEST) -traversal=scan_scan  > scan_cout.txt
	time ../../bin/gong main_terra.t -test_case=$(TEST) -traversal=bvh_bvh    > bvh_cout.txt


clean: