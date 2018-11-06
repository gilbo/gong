
# try to auto-detect and compile against CUDA...
CUDA_DIR ?= /usr/local/cuda
CPP=clang++

run: self_trimesh_scan self_trimesh_bvh
	time ./self_trimesh_scan
	time ./self_trimesh_bvh

self_trimesh_scan.o self_trimesn_scan.h: self_trimesh.t
	../../bin/gong self_trimesh.t -suffix=_scan -traversal=scan_scan

self_trimesh_bvh.o self_trimesn_bvh.h: self_trimesh.t
	../../bin/gong self_trimesh.t -suffix=_bvh -traversal=bvh_bvh

self_trimesh_scan: self_trimesh_scan.o self_trimesh_scan.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         -D GPU_ENABLE \
	         -D USE_SCAN \
	         -o self_trimesh_scan self_trimesh_scan.o main.cpp; \
	else \
	  $(CPP) -std=c++11 \
	         -D USE_SCAN \
	         -o self_trimesh_scan self_trimesh_scan.o main.cpp; \
	fi

self_trimesh_bvh: self_trimesh_bvh.o self_trimesh_scan.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         -D GPU_ENABLE \
	         -D USE_BVH \
	         -o self_trimesh_bvh self_trimesh_bvh.o main.cpp; \
	else \
	  $(CPP) -std=c++11 \
	         -D USE_BVH \
	         -o self_trimesh_bvh self_trimesh_bvh.o main.cpp; \
	fi

clean:
	rm self_trimesh_scan.h \
	   self_trimesh_scan.o \
	   self_trimesh_scan
	rm self_trimesh_bvh.h \
	   self_trimesh_bvh.o \
	   self_trimesh_bvh




