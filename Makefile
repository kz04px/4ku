NAME := 4ku
ifndef CXX
	CXX=g++
endif

ifeq ($(OS), Windows_NT)
	SUFFIX := .exe
	RM = del /F
else
	SUFFIX :=
	RM = rm -f
endif

EXE := $(NAME)$(SUFFIX)

default: full

full: clean
	$(CXX) ./src/main.cpp -O3 -march=native -pthread -o $(EXE)

pgo: clean
	$(CXX) ./src/main.cpp -O3 -march=native -pthread -fprofile-generate -lgcov -o $(EXE)
	./$(EXE) bench
	$(CXX) ./src/main.cpp -O3 -march=native -pthread -fprofile-use -fno-peel-loops -fno-tracer -lgcov -o $(EXE)
	$(RM) main.gcda

mini: clean	
ifeq ($(OS),Windows_NT)
	$(error Mini build not supported for Windows)
endif

	bash ./build-mini.sh

release: clean
	$(CXX) ./src/main.cpp -O3 -march=nehalem -pthread --static -o $(EXE)

clean:
	$(RM) $(EXE)