all: \
  build/exec/Day01\
  build/exec/Day02\
  build/exec/Day03\
  build/exec/Day04

clean:
	rm -rf build

build/exec/Day01:
	idris2 day01/Day01.idr -o Day01 && ./build/exec/Day01

build/exec/Day02:
	idris2 day02/Day02.idr -o Day02 && ./build/exec/Day02

build/exec/Day03:
	idris2 day03/Day03.idr -p contrib -o Day03 && ./build/exec/Day03

build/exec/Day04:
	idris2 day04/Day04.idr -o Day04 && ./build/exec/Day04
