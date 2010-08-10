build: wuerfeln 
	ghc --make wuerfeln.hs
        
run: wuerfeln
	./wuerfeln

clean:
	rm -vf *.o *.hi
	rm -vf **/*.o **/*.hi
