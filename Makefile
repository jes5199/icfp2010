all: build test copy

build:
	cd gen_circuit; ghc --make CreateSubmission
	cd gen_circuit; ghc --make Main
	cd gen_circuit; ghc --make TestSimulator
	cd gen_circuit; ghc --make ParseCars
	cd gen_circuit; ghc --make SolveCars

test:
	cd gen_circuit; ./TestSimulator

copy:
	cd gen_circuit; cp CreateSubmission Main TestSimulator ParseCars SolveCars ../bin

.PHONY: all build copy test
