EXECUTABLE= main
GONGFLAGS= 

CUDA_DIR ?= /usr/local/cuda

CPPFLAGS=-g -std=c++14 -O2

BVHNodeType=0

CPPFLAGS += -DBVHNodeType=$(BVHNodeType)


FCL_HOME ?= ../../../fcl/build
EIGEN_INCLUDE ?= /usr/include/eigen3


CPPFLAGS += -I$(FCL_HOME)/include -I$(FCL_HOME)/../include
CPPFLAGS += -I$(FCL_HOME)/include -I$(FCL_HOME)/../include -I$(EIGEN_INCLUDE)
LFLAGS += -lfcl -L$(FCL_HOME)/lib

LFLAGS += -Wl,-rpath,$(FCL_HOME)/lib

LFLAGS += -lccd -L/usr/local/lib

CPPFLAGS += -I/usr/local/include/eigen3

LFLAGS += -L $(CUDA_DIR)/lib64 -lcuda -lcudart

LFLAGS += -Wl,-rpath=$(CUDA_DIR)/lib64
LFLAGS += -L /home/mmara/projects/gong/include/gong/libs/
LFLAGS += -lcub_wrap
LFLAGS += -Wl,-rpath=/home/mmara/projects/gong/include/gong/libs/


OBJS = build/main.o build/collide.o build/mesh_io.o build/collide_fcl.o

all: main

collide_gong.o: collide.t
	../../bin/gong $(GONGFLAGS) collide.t

collide_gong.h: collide.t
	../../bin/gong $(GONGFLAGS) collide.t

build/%.o: %.cpp collide_gong.h mesh_io.h
	@mkdir -p build
	c++ $(CPPFLAGS) -MP -MD $< -c -o $@


$(EXECUTABLE):	$(OBJS) collide_gong.o
	c++ $^ -o $@ $(LFLAGS)

clean:
	rm main build/main.o build/collide.o collide_gong.o collide_gong.h
