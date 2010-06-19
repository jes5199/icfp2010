import GenCircuit

main = putStrLn $ showCircuit $ constructCircuit $ const_1 `chain` output
