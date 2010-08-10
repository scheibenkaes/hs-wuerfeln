wuerfeln: 
	ghc --make wuerfeln.hs
        
game: wuerfeln
	./wuerfeln

clean:
	rm -f *.o *.hi
	rm -f **/*.o **/*.hi
	rm wuerfeln
