import Simulator
import GenCircuit

import Test.HUnit

test_identity_string = "identity string" ~: (showCircuit $ construct1to1Circuit identity) ~=? expected
    where expected = "8L:\n7R8R0#1R1L,\n0R0L0#2L2R,\n1L1R0#3R3L,\n2R2L0#4L4R,\n3L3R0#5R5L,\n4R4L0#6R6L," ++ 
                     "\n5R5L0#7R7L,\n6R6L0#8R0L,\nX7L0#X0R:\n8L"

test_const0 = "const0" ~: simulate (constructCircuit $ const_0 `chain` output) contest_input ~=? (take 17 $ repeat 0)
test_identity = "identity" ~: simulate (construct1to1Circuit identity) contest_input ~=? contest_input

tests = test [ test_identity_string, test_const0, test_identity ]

main = do
  putStrLn "=========="
  putStrLn "Running unit tests"
  runTestTT tests
