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
