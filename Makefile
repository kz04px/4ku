all:
	g++ ./4ku2/src/main.cpp -O3 -o ./out/4ku
	strip -s ./out/4ku