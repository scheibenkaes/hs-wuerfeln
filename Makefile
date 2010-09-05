build: 
	ghc --make -O2 wuerfeln
        
game: build
	./wuerfeln

local-game: build
	./wuerfeln -s127.0.0.1

build-server:
	ghc --make -O2 -o server Server

server: build-server
	./server

clean:
	rm -f *.o *.hi
	rm -f **/*.o **/*.hi
	rm wuerfeln
	rm server
	rm analyze

analyzer:
	ghc --make -o analyze analyze-log
