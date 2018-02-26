

run: raycaster
	./raycaster

raycaster_gong.o: raycaster.t
	../../bin/gong raycaster.t

raycaster_gong.h: raycaster.t
	../../bin/gong raycaster.t

raycaster: raycaster_gong.o raycaster_gong.h main.cpp off_readwrite.h
	c++ -o raycaster raycaster_gong.o main.cpp

clean:
	rm raycaster_gong.h raycaster_gong.o raycaster