

TERRA_FILES = boxes.t main_terra.t PGS_terra.t prelude.t

run: $(TERRA_FILES)
	time ../../bin/gong main_terra.t -traversal=scan_scan  > scan_cout.txt
	time ../../bin/gong main_terra.t -traversal=bvh_bvh    > bvh_cout.txt


clean: