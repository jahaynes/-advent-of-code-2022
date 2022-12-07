all: \
  Day01\
  Day02\
  Day03\
  Day04\
  Day05\
  Day07

deps: Common/Common.ipkg
	idris2 --install Common/Common.ipkg

clean:
	rm -rf build

Day01: build/exec/Day01
build/exec/Day01: day01/Day01.idr
	idris2 day01/Day01.idr -o Day01

Day02: build/exec/Day02
build/exec/Day02: day02/Day02.idr
	idris2 day02/Day02.idr -o Day02

Day03: build/exec/Day03
build/exec/Day03: day03/Day03.idr
	idris2 day03/Day03.idr -p contrib -o Day03

Day04: build/exec/Day04
build/exec/Day04: day04/Day04.idr
	idris2 day04/Day04.idr -p Common -o Day04

Day05: build/exec/Day05
build/exec/Day05: day05/Day05.idr
	idris2 day05/Day05.idr -p Common -o Day05

Day07: build/exec/Day07
build/exec/Day07: day07/Day07.idr
	idris2 day07/Day07.idr -p contrib -p Common -o Day07
