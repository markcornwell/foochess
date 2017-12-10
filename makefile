# Makefile for foochess

all:
	ghc -O main.hs square.hs
	ghc -O tests.hs square.hs

run:
	./main

test:
	./tests
