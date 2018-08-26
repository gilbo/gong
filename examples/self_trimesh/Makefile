
# try to auto-detect and compile against CUDA...
CUDA_DIR ?= /usr/local/cuda
CPP=clang++

run: self_trimesh
	./self_trimesh

self_trimesh.o: self_trimesh.t
	../../bin/gong self_trimesh.t

self_trimesh.h: self_trimesh.t
	../../bin/gong self_trimesh.t

self_trimesh: self_trimesh.o self_trimesh.h main.cpp
	if [ -d $(CUDA_DIR) ]; then \
	  $(CPP) -std=c++11 \
	         -L $(CUDA_DIR)/lib64 -lcuda -lcudart \
	         -Wl,-rpath=$(CUDA_DIR)/lib64 \
	         -o self_trimesh self_trimesh.o main.cpp; \
	else \
	  $(CPP) -std=c++11 \
	         -o self_trimesh self_trimesh.o main.cpp; \
	fi

clean:
	rm self_trimesh.h self_trimesh.o self_trimesh