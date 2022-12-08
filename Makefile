all:
	g++ ./src/main.cpp -O3 -o ./4ku
	strip -s ./build/4ku