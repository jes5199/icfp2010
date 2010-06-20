all: build test copy

build:
	cd gen_circuit; ghc --make CreateSubmission
	cd gen_circuit; ghc --make Main
	cd gen_circuit; ghc --make TestAll
	cd gen_circuit; ghc --make ParseCars
	cd gen_circuit; ghc --make SolveCars
	cd gen_circuit; ghc --make SolveCar

test:
	cd gen_circuit; ./TestAll

copy:
	cd gen_circuit; cp CreateSubmission Main TestAll ParseCars SolveCars SolveCar ../bin

.PHONY: all build copy test
