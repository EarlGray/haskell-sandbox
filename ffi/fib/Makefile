csrc=test.c
hssrc=FibC.hs
hsobj=$(subst .hs,.o, $(hssrc))
exe=fib

$(exe): $(csrc) $(hsobj)
	ghc --make -no-hs-main -optc-O $(csrc) $(hsobj) -o $(exe)

$(hsobj): $(hssrc)
	ghc -c -O $(hssrc)

.PHONY: clean

clean:
	rm *.hi || true 
	rm *.o || true
	rm *stub.h || true
