

run: self_trimesh
	./self_trimesh

self_trimesh.o: self_trimesh.t
	../../bin/gong self_trimesh.t

self_trimesh.h: self_trimesh.t
	../../bin/gong self_trimesh.t

self_trimesh: self_trimesh.o self_trimesh.h main.cpp
	c++ -o self_trimesh self_trimesh.o main.cpp

