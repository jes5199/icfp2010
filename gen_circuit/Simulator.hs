module Simulator where

import Circuitry

type Trit = Int

-- The input sequence used by the contest organizers appears to be 01202101210201202
contest_input :: [Trit]
contest_input = [0,1,2,0,2,1,0,1,2,1,0,2,0,1,2,0,2]

sim_gate :: [Trit] -> [Trit] -> ([Trit], [Trit])
sim_gate xs ys = splitLists $ zipWith f xs ys
    where f x y = ((x - y) `mod` 3, (x*y + 2) `mod` 3)
          splitLists xs = (map fst xs, map snd xs)

simulate :: Circuit -> [Trit] -> [Trit]
simulate (Just input, gates, Just output) input_sequence = output_sequence
    where gate_outputs = [sim_gate (lookup i in_left) (lookup i in_right)
                              | (i, Gate (Just in_left) (Just in_right) _ _) <- zip [0..] gates]
          lookup i (InternalAddress j LeftSide) | j < i = fst $ gate_outputs !! j
                                                | otherwise = 0 : (fst $ gate_outputs !! j)
          lookup i (InternalAddress j RightSide) | j < i = snd $ gate_outputs !! j
                                                 | otherwise = 0 : (snd $ gate_outputs !! j)
          lookup i ExternalAddress = input_sequence
          output_sequence = lookup (length gates) output

example_circuit :: Circuit
example_circuit = (Just (InternalAddress 19 LeftSide),
                   [Gate (Just (InternalAddress 12 RightSide)) (Just (InternalAddress 13 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 14 RightSide)) (Just (InternalAddress 0 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 9 RightSide)) (Just (InternalAddress 10 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 2 LeftSide)) (Just (InternalAddress 17 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 15 RightSide)) (Just (InternalAddress 1 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 3 LeftSide)) (Just (InternalAddress 18 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 5 LeftSide)) (Just (InternalAddress 11 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 19 RightSide)) (Just (InternalAddress 16 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 2 RightSide)) (Just (InternalAddress 7 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 1 RightSide)) (Just (InternalAddress 3 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 8 RightSide)) (Just (InternalAddress 4 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 8 LeftSide)) (Just (InternalAddress 7 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 6 RightSide)) (Just (InternalAddress 0 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 6 LeftSide)) (Just (InternalAddress 4 RightSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 12 LeftSide)) (Just (InternalAddress 13 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 5 RightSide)) (Just (InternalAddress 11 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 10 LeftSide)) (Just (InternalAddress 15 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 14 LeftSide)) (Just (InternalAddress 16 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (InternalAddress 9 LeftSide)) (Just (InternalAddress 17 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress)),
                   Gate (Just (ExternalAddress)) (Just (InternalAddress 18 LeftSide)) (Just (ExternalAddress)) (Just (ExternalAddress))],
                   Just (InternalAddress 19 LeftSide))

example_input :: [Trit]
example_input = [0,2,2,2,2,2,2,0,2,1,0,1,1,0,0,1,1]

proper_prefix :: [Trit]
proper_prefix = simulate example_circuit example_input
