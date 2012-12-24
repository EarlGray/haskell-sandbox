SRC=so.c

LIBNAME=so
MAJVER=0
MINVER=0

LIB=lib$(LIBNAME).so
LIBMAJ=$(LIB).$(MAJVER)
LIBMAJMIN=$(LIBMAJ).$(MINVER)

EXE=DynLib

.PHONY: run, clean, nuke

run: $(EXE) $(LIB)
	LD_LIBRARY_PATH=. ./$(EXE)

$(LIB):
	$(CC) -shared -fPIC -Wl,-soname,$(LIBNAME) -o $(LIBMAJMIN) $(SRC)
	ln -s $(LIBMAJMIN) $(LIBMAJ)
	ln -s $(LIBMAJMIN) $(LIB)

$(EXE): $(EXE).hs
	ghc $(EXE).hs -ldl

clean:
	rm *.o; rm *.hi; true

nuke: clean
	rm $(EXE) $(LIB) $(LIBMAJ) $(LIBMAJMIN)
