SRC=so.c

LIBNAME=so
MAJVER=0
MINVER=0

LIB=lib$(LIBNAME).so
LIBMAJ=$(LIB).$(MAJVER)
LIBMAJMIN=$(LIBMAJ).$(MINVER)

.PHONY: clean

libhello.so:
	$(CC) -shared -fPIC -Wl,-soname,$(LIBNAME) -o $(LIBMAJMIN) $(SRC)
	ln -s $(LIBMAJMIN) $(LIBMAJ)
	ln -s $(LIBMAJMIN) $(LIB)

clean:
	rm *.o; rm *.hi
