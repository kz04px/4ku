all:
	g++ ./src/main.cpp -O3 -o ./out/4ku.exe
	strip -s ./out/4ku.exe