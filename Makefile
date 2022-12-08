all:
	mkdir -f build
	g++ ./src/main.cpp -O3 -o ./build/4ku
	strip -s ./build/4ku.exe