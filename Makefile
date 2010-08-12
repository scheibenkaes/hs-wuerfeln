build: 
	ghc --make -O2 wuerfeln
        
game: build
	./wuerfeln

clean:
	rm -f *.o *.hi
	rm -f **/*.o **/*.hi
	rm wuerfeln
