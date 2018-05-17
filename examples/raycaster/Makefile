GONGFLAGS= 
CPPFLAGS=-g -std=c++14


run: raycaster
	./raycaster

raycaster_gong.o: raycaster.t
	../../bin/gong $(GONGFLAGS) raycaster.t

raycaster_gong.h: raycaster.t
	../../bin/gong $(GONGFLAGS) raycaster.t

raycaster: raycaster_gong.o raycaster_gong.h main.cpp off_readwrite.h
	c++ $(CPPFLAGS) -o raycaster raycaster_gong.o main.cpp

clean:
	rm raycaster_gong.h raycaster_gong.o raycaster