import Simulator
import GenCircuit
import CarParts
import Solver
import FuelChecker

import Test.HUnit

test_identity_string = "identity string" ~: (showCircuit $ construct1to1Circuit identity) ~=? expected
    where expected = "8L:\n7R8R0#1R1L,\n0R0L0#2L2R,\n1L1R0#3R3L,\n2R2L0#4L4R,\n3L3R0#5R5L,\n4R4L0#6R6L," ++ 
                     "\n5R5L0#7R7L,\n6R6L0#8R0L,\nX7L0#X0R:\n8L"

test_const_n n sub = ("const" ++ show n) ~: simulate (constructCircuit $ sub `chain` output) contest_input ~?= expected
    where expected = take 17 $ repeat n

test_identity = "identity" ~: simulate (construct1to1Circuit identity) contest_input ~?= contest_input

emitter_list = [1,2,0,0,2,0,2,2,2,1,1,2,2,1,2] -- Arbitary
test_emitter = "emitter" ~: ( take (length emitter_list) $ simulate (construct1to1Circuit $ emitter emitter_list) contest_input ) ~?= emitter_list

test_car_solve :: Car -> Maybe Fuel -> Test
test_car_solve car mfuel = name ~: solve car ~?= mfuel
    where name = "test_car_solve " ++ show car ++ " " ++ showsPrec 20 mfuel []

car_solve_tests = "car_solve_tests" ~: test
                  [test_car_solve [] (Just []),
                   test_car_solve [([0,0],0,[0])] (Just [[[2]]]),
                   test_car_solve [([0,0,1],0,[0,1])] (Just [[[2]],[[1]]]),
                   test_car_solve [([0,1,2,3,0,0,0,0],0,[0,1,2,3,0,0])] (Just [[[2]],[[1]],[[1]],[[1]]]),
                   test_car_solve [([0,1,2,3,4,0,0,0,0],0,[0,1,2,3,4,0,0])] (Just [[[2]],[[1]],[[1]],[[1]],[[1]]]),
                   test_car_solve [([0],0,[1]),([2],1,[0]),([1,0],1,[2]),([2,0],1,[0,1,2])] (Just [[[2]],[[1]],[[2]]])
                  ]

test_fuel_checker = "fuel checker" ~: check_fuel [([0,0],0,[0])] [[[2,0],[0,0]]] ~?= Just ()

tests = test [ test_identity_string, test_const_n 0 const_0, test_const_n 1 const_1, test_const_n 2 const_2,
               test_identity, test_emitter, car_solve_tests, test_fuel_checker ]

main = do
  putStrLn "=========="
  putStrLn "Running unit tests"
  runTestTT tests
