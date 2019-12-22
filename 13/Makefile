result.txt:	care
	./care | tee result.txt
care:	care.hs Intcode.hs
	ghc -O -Wall care
clean:
	-rm result.txt care *.hi *.o
