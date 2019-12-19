result.txt:	nbody
	time ./nbody | tee result.txt
nbody:	nbody.hs
	ghc -O -Wall nbody
clean:
	-rm result.txt nbody *.hi *.o
