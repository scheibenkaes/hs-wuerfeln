build: 
	ghc --make -O2 wuerfeln
        
game: build
	./wuerfeln

build-server:
	ghc --make -o server Server

server: build-server
	./server

clean:
	rm -f *.o *.hi
	rm -f **/*.o **/*.hi
	rm wuerfeln
	rm server
