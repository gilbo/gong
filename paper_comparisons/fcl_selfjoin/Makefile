EXECUTABLE= main
GONGFLAGS= 
CPPFLAGS=-g -std=c++14

FCL_HOME ?= ../../../fcl/build


CPPFLAGS += -I$(FCL_HOME)/include -I$(FCL_HOME)/../include
LFLAGS += -lfcl -L$(FCL_HOME)/lib

LFLAGS += -Wl,-rpath,$(FCL_HOME)/lib


CPPFLAGS += -I/usr/local/include/eigen3


OBJS = build/main.o build/collide.o

all: main

collide_scan.o: collide.t
	../../bin/gong $(GONGFLAGS) collide.t

collide_scan.h: collide.t
	../../bin/gong $(GONGFLAGS) collide.t

build/%.o: %.cpp collide_scan.h collide_bvh.h
	@mkdir -p build
	c++ $(CPPFLAGS) -MP -MD $< -c -o $@


$(EXECUTABLE):	$(OBJS) collide_gong.o
	c++ $^ -o $@ $(LFLAGS)

clean:
	rm main build/main.o build/collide.o collide_gong.o collide_gong.h