module Circuitry where

data Side = LeftSide | RightSide
    deriving Eq

data Address = InternalAddress Int Side -- e.g. "3R"
             | ExternalAddress -- "X"
    deriving Eq

instance Show Address where
    show (InternalAddress n s) = show n ++ showSide s
    show ExternalAddress = "X"

-- A gate is represented by the two addresses that the inputs connect
-- to, and the two addresses that the outputs connect to.
data Gate = Gate (Maybe Address) (Maybe Address) (Maybe Address) (Maybe Address)

showAddr Nothing = "__"
showAddr (Just addr) = show addr

showSide LeftSide = "L"
showSide RightSide = "R"

instance Show Gate where
    show (Gate a b c d) = showAddr a ++ showAddr b ++ "0#" ++ showAddr c ++ showAddr d
