

TERRA_FILES = sphere_uniform.t

run: sphere_uniform
	./sphere_uniform


sphere_uniform.o: $(TERRA_FILES)
	../../bin/gong sphere_uniform.t

sphere_uniform.h: $(TERRA_FILES)
	../../bin/gong sphere_uniform.t

sphere_uniform: sphere_uniform.o sphere_uniform.h main.cpp
	c++ -o sphere_uniform sphere_uniform.o main.cpp

clean:
	rm sphere_uniform.h sphere_uniform.o sphere_uniform