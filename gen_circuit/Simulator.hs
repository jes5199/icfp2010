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
