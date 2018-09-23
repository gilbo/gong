

TERRA_FILES = sphere_uniform.t

run: Sphere_Check sphere_uniform_scan sphere_uniform_bvh
	./sphere_uniform_scan
	./sphere_uniform_bvh

# make sure the data to run on is present or present
# a somewhat comprehensible error to the user otherwise
Sphere_Check:
	@if [ -d Spheres ]; then \
	  echo "found Spheres directory"; \
	else \
	  echo "\n  **>> could not find Spheres directory; ASK DEVELOPERS <<**\n"; \
	  exit 1; \
	fi

sphere_uniform_scan.o sphere_uniform_scan.h: $(TERRA_FILES)
	../../bin/gong sphere_uniform.t -suffix=_scan -traversal=scan_scan

sphere_uniform_bvh.o sphere_uniform_bvh.h: $(TERRA_FILES)
	../../bin/gong sphere_uniform.t -suffix=_bvh -traversal=bvh_bvh

sphere_uniform_scan: sphere_uniform_scan.o sphere_uniform_scan.h main.cpp
	c++ -D USE_SCAN -o sphere_uniform_scan sphere_uniform_scan.o main.cpp

sphere_uniform_bvh: sphere_uniform_bvh.o sphere_uniform_bvh.h main.cpp
	c++ -D USE_BVH -o sphere_uniform_bvh sphere_uniform_bvh.o main.cpp

clean:
	rm sphere_uniform_scan.h \
	   sphere_uniform_scan.o \
	   sphere_uniform_scan
	rm sphere_uniform_bvh.h \
	   sphere_uniform_bvh.o \
	   sphere_uniform_bvh