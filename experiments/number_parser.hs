import Numeric
import Data.Char

tritEncode :: Int -> String
tritEncode 0 = "0"
tritEncode 1 = "1"
tritEncode 2 = "220"
tritEncode i = encoded
    where len = 1 + (floor $ logBase 3 (fromInteger $ toInteger $ i+3))
          base = 3 ^ len
          mod = i + 3 - base`div`3
          j = base + mod
          based = inBase3 j
          encoded = (replicate ((len-1) * 2) '2') ++ (drop 1 based)

inBase3 = showIntAtBase 3 intToDigit `flip` ""


parseSingleTritCode :: String -> (Int, String)
parseSingleTritCode [] = (-3, [])
parseSingleTritCode ('0':xs)         = (0,xs)
parseSingleTritCode ('1':xs)         = (1,xs)
parseSingleTritCode ('2':'0':xs)     = (-1,xs)
parseSingleTritCode ('2':'1':xs)     = (-2,xs)
parseSingleTritCode ('2':'2':'0':xs) = (2,xs)
parseSingleTritCode s@('2':'2':xs)   = (parsed,rest)
    where size    = (twos `div` 2) + 1
          parsed  = parsePart digits
          digits  = take size after2s
          rest    = drop size after2s
          after2s = drop twos s
          twos    = howManyTwos s
parseSingleTritCode rest = error $ show rest

parseTritsNaive :: String -> [Int]
parseTritsNaive [] = []
parseTritsNaive ('0':xs)         = 0:parseTritsNaive xs
parseTritsNaive ('1':xs)         = 1:parseTritsNaive xs
parseTritsNaive ('2':'0':xs)     = -1:parseTritsNaive xs
parseTritsNaive ('2':'1':xs)     = -2:parseTritsNaive xs
parseTritsNaive ('2':'2':'0':xs) = 2:parseTritsNaive xs
parseTritsNaive s@('2':'2':xs)   = parsed : parseTritsNaive rest
    where size    = (twos `div` 2) + 1
          parsed  = parsePart digits
          digits  = take size after2s
          rest    = drop size after2s
          after2s = drop twos s
          twos    = howManyTwos s
    
parseTritsNaive rest = error $ show rest

parsePart :: String -> Int
parsePart digits = (asTrinary 0 digits) + (offsetByLength len)
   where len = length digits 

offsetByLength n = 3^(n-1) - 3
         
asTrinary acc [d]    = acc + (num d)
asTrinary acc (d:ds) = asTrinary ( (acc+(num d)) *3) ds 

num '0' = 0
num '1' = 1
num '2' = 2

parseTritArray :: String -> (String, String)
parseTritArray ds = splitAt len rest
    where (len, rest) = parseSingleTritCode ds

parseTritList :: (String -> (a, String)) -> String -> ([a], String)
parseTritList func ds = subparse len rest
    where (len, rest) = parseSingleTritCode ds
          subparse 0 sub_rest = ([], sub_rest)
          subparse n sub_rest = ((func_val : next_val) , next_rest)
            where (next_val, next_rest)   = subparse (n-1) func_rest
                  (func_val, func_rest) = func sub_rest

parseFuel = parseTritList $ parseTritList $ parseTritList $ parseTritArray

parseCar ds = ((first_section, sep, second_section), rest)
    where (second_section, rest) = (parseTritList $ parseTritArray) rest2
          (sep:rest2) = rest1
          (first_section, rest1) = (parseTritList $ parseTritList $ parseTritArray) ds

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
