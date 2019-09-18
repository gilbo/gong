EXECUTABLE= main

GONGFLAGS= 
CPPFLAGS=-g -std=c++14

FCL_HOME ?= ../../../fcl/build


CPPFLAGS += -I$(FCL_HOME)/include -I$(FCL_HOME)/../include
LFLAGS += -lfcl -L$(FCL_HOME)/lib

LFLAGS += -Wl,-rpath,$(FCL_HOME)/lib


CPPFLAGS += -I/usr/local/include/eigen3


OBJS = build/main.o build/collide.o

all: $(EXECUTABLE)

build/%.o: %.cpp
	@mkdir -p build
	c++ $(CPPFLAGS) -MP -MD $< -c -o $@


$(EXECUTABLE):	$(OBJS)
	c++ $^ -o $@ $(LFLAGS)

clean:
	rm main build/main.o build/collide.o