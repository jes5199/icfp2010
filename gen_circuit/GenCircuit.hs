module GenCircuit where

import Maybe
import List
import Circuitry
import Test.HUnit
import Simulator
import CarParts

-- A wire is represented by the address of its origin and the address
-- of its destination.
data Wire = Wire Address Address
origin (Wire x y) = x
destination (Wire x y) = y

-- A subcircuit is represented by a size (in gates), a number of
-- inputs, a number of outputs, and a function.  The function takes a
-- gate offset (indicating the number of the first gate belonging to
-- the subcircuit), and returns a list of wires, a list of input
-- addresses, and a list of output addresses.
data SubCircuit = SubCircuit Int Int Int (Int -> ([Wire], [Address], [Address]))

-- Chain two subcircuits by connecting the outputs of the first to the
-- inputs of the second.
chain :: SubCircuit -> SubCircuit -> SubCircuit
--chain (SubCircuit size1 ins1 outs1 f1) (SubCircuit size2 ins2 outs2 f2)
--  = SubCircuit (size1 + size2) ins1 outs2 f_composite
--    where f_composite offset = (wires1 ++ wires2 ++ connections, in1_addrs,
--                                out2_addrs)
--              where (wires1, in1_addrs, out1_addrs) = f1 offset
--                    (wires2, in2_addrs, out2_addrs) = f2 (offset + size1)
--                    connections = zipWith Wire out1_addrs in2_addrs
chain c1@(SubCircuit _ _ outs1 _) c2@(SubCircuit _ ins2 _ _)
    | outs1 == ins2 = gen_chain c1 [(n, n) | n <- [0..(outs1-1)]] c2
    | otherwise = error "Tried to chain subcircuits with incompatible arity"

gen_chain :: SubCircuit -> [(Int, Int)] -> SubCircuit -> SubCircuit
gen_chain (SubCircuit size1 ins1 outs1 f1) selector (SubCircuit size2 ins2 outs2 f2)
    = SubCircuit (size1 + size2) (ins1 + ins2 - length selector) (outs2 + outs1 - length selector) f_composite
    where f_composite offset = (wires1 ++ wires2 ++ connections,
                                in1_addrs ++ [in2_addrs !! n | n <- remaining_inputs2],
                                [out1_addrs !! n | n <- remaining_outputs1] ++ out2_addrs)
              where (wires1, in1_addrs, out1_addrs) = f1 offset
                    (wires2, in2_addrs, out2_addrs) = f2 (offset + size1)
                    connections = [Wire (out1_addrs !! m) (in2_addrs !! n) | (m, n) <- selector]
                    remaining_inputs2 = [0..(ins2-1)] \\ map snd selector
                    remaining_outputs1 = [0..(outs1-1)] \\ map fst selector

-- Chain two subcircuits with a delay in between.
chain_delay :: SubCircuit -> SubCircuit -> SubCircuit
chain_delay (SubCircuit size1 ins1 outs1 f1) (SubCircuit size2 ins2 outs2 f2)
  = SubCircuit (size1 + size2) ins1 outs2 f_composite
    where f_composite offset = (wires1 ++ wires2 ++ connections, in1_addrs,
                                out2_addrs)
              where (wires1, in1_addrs, out1_addrs) = f1 (offset + size2)
                    (wires2, in2_addrs, out2_addrs) = f2 offset
                    connections = zipWith Wire out1_addrs in2_addrs

-- Subcircuit representing a single gate.
gate :: SubCircuit
gate = SubCircuit 1 2 2 f
    where f offset = ([],
                      [InternalAddress offset LeftSide,
                       InternalAddress offset RightSide],
                      [InternalAddress offset LeftSide,
                       InternalAddress offset RightSide])

-- Subcircuit representing the output of the final circuit.
output :: SubCircuit
output = SubCircuit 0 1 0 f
    where f offset = ([], [ExternalAddress], [])

-- Subcircuit representing the input of the final circuit.
input :: SubCircuit
input = SubCircuit 0 0 1 f
    where f offset = ([], [], [ExternalAddress])

-- Rearrange the inputs to a subcircuit so that they appear in the
-- order given by input_selector.  For example, if there are three
-- inputs and input_selector == [1, 2, 0], this rotates the inputs so
-- that the input that was previously first is now last.  It is
-- permissible for input_selector to be smaller than the number of
-- inputs to the sub-circuit.  In this case, the inputs that aren't
-- mentioned by input_selector are considered "junk" inputs, and will
-- be connected to arbitrary outputs.
select_inputs :: [Int] -> SubCircuit -> SubCircuit
select_inputs input_selector (SubCircuit size ins outs f)
    = SubCircuit size (length input_selector) outs f'
    where f' offset = (wires, [in_addrs !! n | n <- input_selector], out_addrs)
              where (wires, in_addrs, out_addrs) = f offset

-- Rearrange the outputs to a subcircuit so that they appear in the
-- order given by output_selector.  For example, if there are three
-- outputs and output_selector == [1, 2, 0], this rotates the outputs so
-- that the output that was previously first is now last.  It is
-- permissible for output_selector to be smaller than the number of
-- outputs to the sub-circuit.  In this case, the outputs that aren't
-- mentioned by input_selector are considered "junk" outputs, and will
-- be connected to arbitrary inputs.
select_outputs :: [Int] -> SubCircuit -> SubCircuit
select_outputs output_selector (SubCircuit size ins outs f)
    = SubCircuit size ins (length output_selector) f'
    where f' offset = (wires, in_addrs, [out_addrs !! n | n <- output_selector])
              where (wires, in_addrs, out_addrs) = f offset

-- Swap the outputs of a two-output subcircuit.
swap_outs = select_outputs [1, 0]

swapped_gate :: SubCircuit
swapped_gate = swap_outs gate

-- Subcircuit that outputs the constant value 0
const_0 :: SubCircuit
const_0 = select_outputs [0] $ select_inputs [] inner
    where inner = swapped_gate `chain` gate `chain` swapped_gate `chain` gate `chain`
                  swapped_gate `chain` swapped_gate `chain` swapped_gate `chain` gate

const_2 :: SubCircuit
const_2 = select_outputs [1] $ select_inputs [] $ gen_chain const_0 [(0, 0)] gate

const_1 :: SubCircuit
const_1 = select_outputs [0] $ select_inputs [] $ gen_chain const_0 [(0, 0)] $ gen_chain const_2 [(0, 1)] gate

identity :: SubCircuit
identity = select_outputs [0] $ gen_chain const_0 [(0, 1)] gate

plus_0 = identity

plus_1 :: SubCircuit
plus_1 = select_outputs [0] $ gen_chain const_2 [(0, 1)] gate

plus_2 :: SubCircuit
plus_2 = select_outputs [0] $ gen_chain const_1 [(0, 1)] gate

plus_ :: Int -> SubCircuit
plus_ 0 = plus_0
plus_ 1 = plus_1
plus_ 2 = plus_2
plus_ x = plus_ (x `mod` 3)

const_ :: Int -> SubCircuit
const_ 0 = const_0
const_ 1 = const_1
const_ 2 = const_2
const_ x = const_ (x `mod` 3)


fix_junk :: SubCircuit -> SubCircuit
fix_junk (SubCircuit size ins outs f) = SubCircuit size 0 0 f'
    where f' 0 = (wires ++ new_wires, [], [])
              where (wires, in_addrs, out_addrs) = f 0
                    unconnected_ins = all_possible_addresses \\ map destination wires
                    unconnected_outs = all_possible_addresses \\ map origin wires
                    all_possible_addresses
                        = [ExternalAddress] ++ concat [[InternalAddress g LeftSide, InternalAddress g RightSide]
                                                           | g <- [0..(0+size-1)]]
                    new_wires = zipWith Wire unconnected_outs unconnected_ins
          f' _ = error "fix_junk called with nonzero offset"

render_circuit :: SubCircuit -> Circuit
render_circuit (SubCircuit size ins outs f) = (whole_circuit_input,
                                               [gate_g g | g <- [0..(size-1)]],
                                               whole_circuit_output)
    where (wires, in_addrs, out_addrs) = f 0
          gate_g_l_input g = listToMaybe $ map origin $ filter (\w -> destination w == InternalAddress g LeftSide) wires
          gate_g_r_input g = listToMaybe $ map origin $ filter (\w -> destination w == InternalAddress g RightSide) wires
          gate_g_l_output g = listToMaybe $ map destination $ filter (\w -> origin w == InternalAddress g LeftSide) wires
          gate_g_r_output g = listToMaybe $ map destination $ filter (\w -> origin w == InternalAddress g RightSide) wires
          gate_g g = Gate (gate_g_l_input g) (gate_g_r_input g) (gate_g_l_output g) (gate_g_r_output g)
          whole_circuit_input = listToMaybe $ map destination $ filter (\w -> origin w == ExternalAddress) wires
          whole_circuit_output = listToMaybe $ map origin $ filter (\w -> destination w == ExternalAddress) wires

showCircuit :: (Maybe Address, [Gate], Maybe Address) -> String
showCircuit (circuit_in, gates, circuit_out) = showAddr circuit_in ++ ":\n" ++
                                               foldl (++) "" (intersperse ",\n" $ map show gates) ++
                                               ":\n" ++ showAddr circuit_out

constructCircuit :: SubCircuit -> Circuit
constructCircuit = render_circuit . fix_junk

emitterPlanner :: [Int] -> [Int]
emitterPlanner []     = []
emitterPlanner (x:xs) = x:(emitterPlanner $ map (\n-> (n-x) `mod` 3) xs) 

emitter :: [Int] -> SubCircuit
--emitter xs = foldl1 (flip chain_delay) (map plus_ plan)
--    where plan = emitterPlanner xs
emitter = bi_emitter

construct1to1Circuit :: SubCircuit -> Circuit
construct1to1Circuit sub = constructCircuit $ input `chain` sub `chain` output

-- Create a circuit that cycles n steps through the patterns (00, 20, 22, ...)
bi_plus :: Int -> SubCircuit
bi_plus 1 = swapped_gate
bi_plus 2 = swapped_gate `chain` swapped_gate
bi_plus 0 = swapped_gate `chain` swapped_gate `chain` swapped_gate

-- A circuit that converts 00 -> 1, 20 -> 2, 22 -> 0
bi_to_single :: SubCircuit
bi_to_single = select_outputs [0] $ select_inputs [1, 0] $ gate `chain` gate

bi_emitter :: [Int] -> SubCircuit
bi_emitter xs = select_inputs [0] $ foldl1 (flip chain_delay) (map bi_plus plan) `chain` bi_to_single
    where plan = emitterPlanner (map (\n -> (n+2) `mod` 3) xs)

compileCircuit :: [Int] -> String
compileCircuit = showCircuit . construct1to1Circuit . emitter . (proper_prefix++)

