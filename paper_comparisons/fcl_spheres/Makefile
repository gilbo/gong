EXECUTABLE= main
GONGFLAGS=


CPPFLAGS=-g -std=c++14 -O2

FCL_HOME ?= ../../../fcl/build
EIGEN_INCLUDE ?= /usr/include/eigen3

CPPFLAGS += -I$(FCL_HOME)/include -I$(FCL_HOME)/../include
CPPFLAGS += -I$(FCL_HOME)/include -I$(FCL_HOME)/../include -I$(EIGEN_INCLUDE)
LFLAGS += -lfcl -L$(FCL_HOME)/lib

LFLAGS += -Wl,-rpath,$(FCL_HOME)/lib

LFLAGS += -lccd -L/usr/local/lib
CPPFLAGS += -I/usr/local/include/eigen3


OBJS = build/main.o build/collide_fcl.o

all: main

build/%.o: %.cpp timer.h config.h collide_fcl.h
	@mkdir -p build
	c++ $(CPPFLAGS) -MP -MD $< -c -o $@


$(EXECUTABLE):	$(OBJS)
	c++ $^ -o $@ $(LFLAGS)

clean:
	rm main build/main.o
