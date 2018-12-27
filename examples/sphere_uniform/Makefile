
# try to auto-detect and compile against CUDA...
CUDA_DIR ?= /usr/local/cuda
CPP=clang++

TERRA_FILES = sphere_uniform.t

run: Sphere_Check sphere_uniform_scan sphere_uniform_gpu_scan sphere_uniform_bvh
	./sphere_uniform_scan
	if [ -d $(CUDA_DIR) ]; then \
	  ./sphere_uniform_gpu_scan; \
	fi
	./sphere_uniform_bvh

# make sure the data to run on is present or present
# a somewhat comprehensible error to the user otherwise
Sphere_Check:
	@if [ -d Spheres ]; then \
	  echo "found Spheres directory"; \
	else \
	  echo "\n  **>> could not find Spheres directory; ASK DEVELOPERS <<**\n"; \
	  exit 1; \
	fi

sphere_uniform_scan.o sphere_uniform_scan.h: $(TERRA_FILES)
	../../bin/gong sphere_uniform.t -suffix=_scan -traversal=scan_scan

sphere_uniform_bvh.o sphere_uniform_bvh.h: $(TERRA_FILES)
	../../bin/gong sphere_uniform.t -suffix=_bvh -traversal=bvh_bvh

sphere_uniform_scan: sphere_uniform_scan.o sphere_uniform_scan.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         -D USE_SCAN \
	         -o sphere_uniform_scan sphere_uniform_scan.o main.cpp; \
	else \
	  $(CPP) -std=c++11 \
	         -D USE_SCAN \
	         -o sphere_uniform_scan sphere_uniform_scan.o main.cpp; \
	fi

sphere_uniform_gpu_scan: sphere_uniform_scan.o sphere_uniform_scan.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         -D GPU_ENABLE \
	         -D USE_SCAN \
	         -o sphere_uniform_gpu_scan sphere_uniform_scan.o main.cpp; \
	fi

sphere_uniform_bvh: sphere_uniform_bvh.o sphere_uniform_bvh.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         -D USE_BVH \
	         -o sphere_uniform_bvh sphere_uniform_bvh.o main.cpp; \
	else \
	  $(CPP) -std=c++11 \
	         -D USE_BVH \
	         -o sphere_uniform_bvh sphere_uniform_bvh.o main.cpp; \
	fi

clean:
	rm sphere_uniform_scan.h \
	   sphere_uniform_scan.o \
	   sphere_uniform_scan
	rm sphere_uniform_bvh.h \
	   sphere_uniform_bvh.o \
	   sphere_uniform_bvh