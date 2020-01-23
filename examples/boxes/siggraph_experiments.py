#!/usr/bin/env python3

import os

os.system("mkdir -p exp_out")
os.system("mkdir -p exp_out/bullet_blocks_out")
os.system("mkdir -p exp_out/gong_cpu_scan")
os.system("mkdir -p exp_out/gong_gpu_scan")
os.system("mkdir -p exp_out/gong_cpu_bvh")
os.system("mkdir -p exp_out/gong_gpu_bvh")
os.system("mkdir -p exp_out/gong_cpu_hash")


height = 25
radius = 24

def run_case(height,radius):
  os.system("echo '\nRunning height=%s radius=%s\n'" % (height,radius))
  data_dir = ("exp_out/bullet_blocks_out/round_tower_h%d_r%d"
              % (height,radius))
  exp_name = "round_tower_out_h%d_r%d" % (height,radius)
  os.system("mkdir -p " + data_dir)

  os.system("""time ../../../bullet_sims/bullet_blocks -h%d -r%d \
    -f%s/frame \
    > exp_out/bullet_blocks_out/%s.txt
  """ % (height,radius,data_dir,exp_name))

  os.system("""time ../../bin/gong main_terra.t \
    -test_case=round_tower -traversal=bvh_bvh -gpu=true \
    -tower_levels=%d -tower_ring=%d \
    -load_dir=%s \
    > exp_out/gong_gpu_bvh/%s.txt
  """ % (height,radius,data_dir,exp_name))

  os.system("""time ../../bin/gong main_terra.t \
    -test_case=round_tower \
    -tower_levels=%d -tower_ring=%d \
    -load_dir=%s \
    > exp_out/gong_cpu_scan/%s.txt
  """ % (height,radius,data_dir,exp_name))

  os.system("""time ../../bin/gong main_terra.t \
    -test_case=round_tower -traversal=bvh_bvh -gpu=true \
    -tower_levels=%d -tower_ring=%d \
    -load_dir=%s \
    > exp_out/gong_gpu_bvh/%s.txt
  """ % (height,radius,data_dir,exp_name))

  os.system("""time ../../bin/gong main_terra.t \
    -test_case=round_tower -gpu=true \
    -tower_levels=%d -tower_ring=%d \
    -load_dir=%s \
    > exp_out/gong_gpu_scan/%s.txt
  """ % (height,radius,data_dir,exp_name))

  os.system("""time ../../bin/gong main_terra.t \
    -test_case=round_tower -traversal=hash_split \
    -tower_levels=%d -tower_ring=%d \
    -load_dir=%s \
    > exp_out/gong_cpu_hash/%s.txt
  """ % (height,radius,data_dir,exp_name))


run_case(25,24)
run_case(50,24)
run_case(50,48)
run_case(100,48)
run_case(100,96)
run_case(200,96)
run_case(200,192)



