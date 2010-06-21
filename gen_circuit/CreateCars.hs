import Random
import Control.Monad
import System.IO.Unsafe

getRandomValue :: Random a => IO a
getRandomValue = do gen <- newStdGen
                    return (fst $ random gen)

matrixOf :: Int -> [Integer] -> [[Integer]]
matrixOf size input = take size slices
    where slices = slice (1:input)
          slice list = (take size list):(slice (drop size list))

main = do val <- sequence [(getRandomValue :: IO Integer) | i <- [1..10]]
          print $ matrixOf 3 val
