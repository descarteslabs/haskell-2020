all: puzzle_hs puzzle_c puzzle_cc

puzzle_hs: puzzle.hs
	ghc -O2 puzzle.hs
	mv puzzle puzzle_hs

puzzle_c: puzzle.c
	gcc -O2 puzzle.c -o puzzle_c

puzzle_cc: puzzle.cc
	g++ -O2 -std=c++14 puzzle.cc -o puzzle_cc
