
# try to auto-detect and compile against CUDA...
CUDA_DIR ?= /usr/local/cuda
CPP=clang++

TERRA_FILES = sphere_uniform.t


CUDA_DIR ?= /usr/local/cuda

ifeq ($(wildcard $(CUDA_DIR)/b*n),)
  $(info Did not find CUDA_DIR)
else
  $(info Found CUDA_DIR!)
  CUB_WRAP_DIR := $(realpath ../../include/gong/libs/)
  ifeq ($(wildcard $(CUB_WRAP_DIR)/libcub_wrap.so),)
  	$(error Could not find cub_wrap.so)
  endif
  GPU_CUB_FLAGS := -L $(CUB_WRAP_DIR) -lcub_wrap -Wl,-rpath=$(CUB_WRAP_DIR)
endif

run: Sphere_Check sphere_uniform_gpu_scan sphere_uniform_gpu_bvh sphere_uniform_bvh sphere_uniform_hash
	if [ -d $(CUDA_DIR) ]; then \
		echo "Running GPU SCAN"; \
	  ./sphere_uniform_gpu_scan; \
		echo "Running GPU BVH"; \
	  ./sphere_uniform_gpu_bvh; \
	fi
	echo "Running CPU BVH"; \
	./sphere_uniform_bvh
	echo "Running CPU HASH"; \
	./sphere_uniform_hash

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

sphere_uniform_hash.o sphere_uniform_hash.h: $(TERRA_FILES)
	../../bin/gong sphere_uniform.t -suffix=_hash -traversal=hash_scan

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
	         $(GPU_CUB_FLAGS) \
	         -D USE_SCAN \
	         -o sphere_uniform_gpu_scan sphere_uniform_scan.o main.cpp; \
	fi

sphere_uniform_gpu_bvh: sphere_uniform_bvh.o sphere_uniform_bvh.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         -D GPU_ENABLE \
	         $(GPU_CUB_FLAGS) \
	         -D USE_SCAN \
	         -o sphere_uniform_gpu_bvh sphere_uniform_bvh.o main.cpp; \
	fi

sphere_uniform_bvh: sphere_uniform_bvh.o sphere_uniform_bvh.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         $(GPU_CUB_FLAGS) \
	         -D USE_BVH \
	         -o sphere_uniform_bvh sphere_uniform_bvh.o main.cpp; \
	else \
	  $(CPP) -std=c++11 \
	         -D USE_BVH \
	         -o sphere_uniform_bvh sphere_uniform_bvh.o main.cpp; \
	fi

sphere_uniform_hash: sphere_uniform_hash.o sphere_uniform_hash.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         -D USE_HASH \
	         -o sphere_uniform_hash sphere_uniform_hash.o main.cpp; \
	else \
	  $(CPP) -std=c++11 \
	         -D USE_HASH \
	         -o sphere_uniform_hash sphere_uniform_hash.o main.cpp; \
	fi

clean:
	rm sphere_uniform_scan.h \
	   sphere_uniform_scan.o \
	   sphere_uniform_scan \
	   sphere_uniform_gpu_scan
	rm sphere_uniform_bvh.h \
	   sphere_uniform_bvh.o \
	   sphere_uniform_bvh \
	   sphere_uniform_gpu_bvh
	rm sphere_uniform_hash.h \
	   sphere_uniform_hash.o \
	   sphere_uniform_hash \