NAME := 4ku

ifeq ($(OS), Windows_NT)
	SUFFIX := .exe
else
	SUFFIX :=
endif

EXE := $(NAME)$(SUFFIX)

all:
	g++ ./src/main.cpp -O3 -pthread -o $(EXE)
	strip -s ./build/4ku
