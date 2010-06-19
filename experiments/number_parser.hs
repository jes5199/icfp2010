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
         
asTrinary acc []     = acc
asTrinary acc [d]    = acc + (num d)
asTrinary acc (d:ds) = asTrinary ( (acc+(num d)) *3) ds 

num '0' = 0
num '1' = 1
num '2' = 2

intSchemeTwo :: String -> Integer
intSchemeTwo s = x + tri
    where len = length s
          x   = (3^len - 1) `div` 2
          tri = asTrinary 0 s

parseTritArray :: String -> (String, String)
parseTritArray ds = splitAt len rest
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

parseFuel = parseTritList $ parseTritList $ parseTritList $ parseIntSchemeTwo

parseCar = parseTritList $ parseCylinder
    where parseCylinder ds = ((upper_pipe, main_flag, lower_pipe), rest3)
           where (upper_pipe,rest1) = parseTritList parseIntSchemeTwo ds
                 (main_flag:rest2)  = rest1
                 (lower_pipe,rest3) = parseTritList parseIntSchemeTwo rest2

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
