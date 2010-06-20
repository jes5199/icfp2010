module CarParts where

type Car = [ReactionChamber]

type ReactionChamber = (Pipe, Integer, Pipe)

type Pipe = [Integer]

type Fuel = [[[Integer]]]
