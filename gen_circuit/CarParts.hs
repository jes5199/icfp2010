module CarParts where

type Car = [ReactionChamber]

type ReactionChamber = (Pipe, Integer, Pipe)

type Pipe = [Integer]

type Fuel = [[[Integer]]]

num_required_tanks :: Car -> Integer
num_required_tanks car = max0 [max (max0plus1 upper) (max0plus1 lower) | (upper, flag, lower) <- car]
    where max0 xs = maximum (0:xs)
          max0plus1 = max0 . map (1+)
