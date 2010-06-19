parseTrits :: String -> [Int]
parseTrits [] = []
parseTrits ('0':xs)         = 0:parseTrits xs
parseTrits ('1':xs)         = 1:parseTrits xs
parseTrits ('2':'0':xs)     = -1:parseTrits xs
parseTrits ('2':'1':xs)     = -2:parseTrits xs
parseTrits ('2':'2':'0':xs) = 2:parseTrits xs
parseTrits s@('2':'2':xs)   = parsed : parseTrits rest
    where size    = (twos `div` 2) + 1
          parsed  = parsePart digits
          digits  = take size after2s
          rest    = drop size after2s
          after2s = drop twos s
          twos    = howManyTwos s
    
parseTrits rest = error $ show rest

parsePart :: String -> Int
parsePart digits = (asTrinary 0 digits) + (offsetByLength len)
   where len = length digits 

offsetByLength n = 3^(n-1) - 3
         
asTrinary acc [d]    = acc + (num d)
asTrinary acc (d:ds) = asTrinary ( (acc+(num d)) *3) ds 

num '0' = 0
num '1' = 1
num '2' = 2

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
