
mkdir -p exp_out

time ../../bin/gong main_terra.t \
  -test_case=round_tower0 \
  > exp_out/round_tower0_cpu_scan.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower1 \
  > exp_out/round_tower1_cpu_scan.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower2 \
  > exp_out/round_tower2_cpu_scan.txt



time ../../bin/gong main_terra.t \
  -test_case=round_tower0 -gpu=true \
  > exp_out/round_tower0_GPU_scan.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower1 -gpu=true \
  > exp_out/round_tower1_GPU_scan.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower2 -gpu=true \
  > exp_out/round_tower2_GPU_scan.txt



time ../../bin/gong main_terra.t \
  -test_case=round_tower0 -traversal=bvh_bvh  \
  > exp_out/round_tower0_cpu_bvh.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower1 -traversal=bvh_bvh  \
  > exp_out/round_tower1_cpu_bvh.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower2 -traversal=bvh_bvh  \
  > exp_out/round_tower2_cpu_bvh.txt



time ../../bin/gong main_terra.t \
  -test_case=round_tower0 -traversal=bvh_bvh -gpu=true \
  > exp_out/round_tower0_GPU_bvh.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower1 -traversal=bvh_bvh -gpu=true \
  > exp_out/round_tower1_GPU_bvh.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower2 -traversal=bvh_bvh -gpu=true \
  > exp_out/round_tower2_GPU_bvh.txt



time ../../bin/gong main_terra.t \
  -test_case=round_tower0 -traversal=hash_split  \
  > exp_out/round_tower0_cpu_hash.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower1 -traversal=hash_split  \
  > exp_out/round_tower1_cpu_hash.txt

time ../../bin/gong main_terra.t \
  -test_case=round_tower2 -traversal=hash_split  \
  > exp_out/round_tower2_cpu_hash.txt


##



##








#time ../../bin/gong main_terra.t \
#  -test_case=plank_tower2 -traversal=bvh_bvh  \
#  > exp_out/plank_tower2_cpu_bvh.txt

#time ../../bin/gong main_terra.t \
#  -test_case=plank_tower2 -traversal=bvh_bvh  \
#  > exp_out/plank_tower2_cpu_bvh.txt



#time ../../bin/gong main_terra.t \
#  -test_case=round_tower1 -gpu=true \
#  > cout_round_tower1_gpu_scan.txt
#
#time ../../bin/gong main_terra.t \
#  -test_case=round_tower2 -gpu=true \
#  > cout_round_tower2_gpu_scan.txt
