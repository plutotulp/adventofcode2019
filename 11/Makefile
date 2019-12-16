result.txt:	painter
	./painter | tee result.txt

painter:	painter.hs Robot.hs Intcode.hs
	ghc -O -Wall painter

.PHONY: clean
clean:
	-rm result.txt painter *.hi *.o
