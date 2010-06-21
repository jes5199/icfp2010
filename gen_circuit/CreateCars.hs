import Random
import Control.Monad
import System.IO.Unsafe

matrixOf :: Integer -> [Integer] -> [[Integer]]
matrixOf size input = take size slices
    where slices = slice (1:input)
          slice list = (take size list):(slice (drop size list))

main = do
   gen <- newStdGen
   let ns = map (`mod` 3) $ randoms gen :: [Integer]
   print $ matrixOf 3 ns
