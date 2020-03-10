all: build run

build:
	stack build

run:
	stack exec functional-data-structures-exe
