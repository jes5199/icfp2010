module NumberParser where
import Numeric
import Data.Char
import List
import CarParts
import Matrices

tritEncode :: Integer -> String
tritEncode 0 = "0"
tritEncode 1 = "1"
tritEncode n = "22" ++ tritEncode num_digits ++ inBase3 num_digits (n - ((3^num_digits - 1) `div` 2 + 2))
    where num_digits = intLogBase3 $ fromInteger $ toInteger $ 2*(n-2) + 1

intLogBase3 n | n < 3 = 0
              | otherwise = 1 + intLogBase3 (n `div` 3)

inBase3 :: Integer -> Integer -> String
inBase3 0 n = ""
inBase3 digits n = inBase3 (digits-1) (n `div` 3) ++ [intToDigit (fromInteger $ n `mod` 3)]


parseSingleTritCode :: String -> (Integer, String)
parseSingleTritCode ('0':r) = (0, r)
parseSingleTritCode ('1':r) = (1, r)
parseSingleTritCode ('2':'2':r) = (fromInteger ((3^num_digits - 1) `div` 2 + 2 + asTrinary digits), r3)
    where (num_digits, r2) = parseSingleTritCode r
          (digits, r3) = genericSplitAt num_digits r2
parseSingleTritCode ('2':_) = error "Invalid number"

asTrinary :: String -> Integer
asTrinary = asTrinary' 0
    where asTrinary' acc []     = acc
          asTrinary' acc [d]    = acc + (num d)
          asTrinary' acc (d:ds) = asTrinary' ( (acc+(num d)) *3) ds 

num '0' = 0
num '1' = 1
num '2' = 2

intSchemeTwo :: String -> Integer
intSchemeTwo s = x + tri
    where len = length s
          x   = (3^len - 1) `div` 2
          tri = asTrinary s

compileSchemeTwo :: Integer -> String
compileSchemeTwo x = inBase3 len $ x - offset
    where len = intLogBase3 $ 2*x + 1
          offset = (3^len - 1) `div` 2

parseTritArray :: String -> (String, String)
parseTritArray ds = genericSplitAt len rest
    where (len, rest) = parseSingleTritCode ds

parseIntSchemeTwo :: String -> (Integer, String)
parseIntSchemeTwo ds = (intSchemeTwo $ val, rest)
    where (val, rest) = parseTritArray ds

parseTritList :: (String -> (a, String)) -> String -> ([a], String)
parseTritList func ds = subparse len rest
    where (len, rest) = parseSingleTritCode ds
          subparse 0 sub_rest = ([], sub_rest)
          subparse n sub_rest = ((func_val : next_val) , next_rest)
            where (next_val, next_rest)   = subparse (n-1) func_rest
                  (func_val, func_rest) = func sub_rest

parseFuel :: String -> (Fuel, String)
parseFuel = parseTritList $ parseTritList $ parseTritList $ parseIntSchemeTwo

parseCar = parseTritList $ parseCylinder
    where parseCylinder ds = ((upper_pipe, main_flag, lower_pipe), rest3)
           where (upper_pipe,rest1) = parseTritList parseIntSchemeTwo ds
                 (main_flag,rest2)  = parseIntSchemeTwo rest1
                 (lower_pipe,rest3) = parseTritList parseIntSchemeTwo rest2

prettyCar :: Car -> String
prettyCar car = concat [ "Chamber " ++ show i ++ " (" ++ show flag ++ ")" ++ "\n  upper pipe: " ++ show upper ++ "\n  lower pipe: " ++ show lower ++ "\n" | ((upper,flag,lower), i) <- zip car [0..] ]

showCarAsEquations :: Car -> String
showCarAsEquations = concat . intersperse "\n" . map chamberToEquation
    where chamberToEquation (upper, flag, lower) = showPipe upper ++ "x " ++ (if flag == 0 then ">" else ">=") ++
                                                   " " ++ showPipe lower ++ "x"
          showPipe = map showFuel
          showFuel = toEnum . (fromEnum 'A' +) . fromInteger

showFuelAsMatrices :: Fuel -> String
showFuelAsMatrices fuel = concat [[name] ++ ":\n" ++ showMatrix value | (name, value) <- zip ['A'..] fuel]

--  3 10   +0
--  4 11 
--  5 12
--  6 000  +6
--  7 001
--  8 002
--  9 010
--  10 011
--  11 012
--  12 020
--  23 122
--  24 0000 +24

howManyTwos = length . takeWhile (\x -> x == '2')

compileList :: (a -> String) -> [a] -> String
compileList subCompiler xs = tritEncode (genericLength xs) ++ concatMap subCompiler xs

renderSchemeTwo :: Integer -> String
renderSchemeTwo n = tritEncode (genericLength s) ++ s
    where s = compileSchemeTwo n

compileFuel :: Fuel -> String
compileFuel = compileList $ compileList $ compileList $ renderSchemeTwo
