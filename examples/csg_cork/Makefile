

TERRA_FILES = cork_isct.t prelude.t ext.t bignums.t

WARN:
	echo "EXAMPLE REQUIRES AUTO-PATCHING the CORK library."
	echo "  todo: implement that functionality"

run: CORK_LIB
	cd cork; \
	  ./bin/cork -resolve ../triA.off ../triB.off ../triC.off; \
	  cd ..

CORK_LIB: cork/obj/cork_isct.o cork/src/cork_isct.h
	cd cork; make; cd ..

cork/obj/cork_isct.o \
cork/debug/cork_isct.o \
cork/src/cork_isct.h: cork cork_isct.o cork_isct.h
	cp cork_isct.h cork/src/
	mkdir -p cork/obj
	mkdir -p cork/debug
	cp cork_isct.o cork/obj/
	cp cork_isct.o cork/debug/

cork_isct.o: $(TERRA_FILES)
	../../bin/gong cork_isct.t

cork_isct.h: $(TERRA_FILES)
	../../bin/gong cork_isct.t

# download a copy of the library
cork:
	git clone git@github.com:gilbo/cork.git
	echo "YOU NEED TO EDIT THE cork/makeConstants FILE BY HAND"
	echo "** Then, go into the cork directory and type 'make' **"


clean:
	rm cork_isct.h cork_isct.o
	rm cork/obj/cork_isct.o cork/debug/cork_isct.o cork/src/cork_isct.h
	cd cork; make clean; cd ..

purge: clean
	rm -r cork


