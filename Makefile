all: build test copy

build:
	cd gen_circuit; ghc --make CreateSubmission
	cd gen_circuit; ghc --make Main
	cd gen_circuit; ghc --make TestSimulator

test:
	cd gen_circuit; ./TestSimulator

copy:
	cd gen_circuit; cp CreateSubmission Main TestSimulator ../bin

.PHONY: all build copy test