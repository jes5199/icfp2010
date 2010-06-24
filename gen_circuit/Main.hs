import GenCircuit
import Simulator

--main = putStrLn $ showCircuit $ constructCircuit $ const_1 `chain` output
--main = print $ simulate example_circuit example_input
main = putStrLn $ showCircuit $ construct1to1Circuit $ emitter proper_prefix
