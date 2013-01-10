all:
	ghc --make -O2 Main.hs
	./Main -o images/report1.html

clean:
	rm -f *.hi *.o Main
