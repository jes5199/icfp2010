import Random
import Control.Monad
import System.IO.Unsafe

randomElement :: StdGen -> (Integer, StdGen)
randomElement gen = randomR (0,1) gen

randomList :: Int -> (StdGen -> (a, StdGen)) -> StdGen -> ([a], StdGen)
randomList 0   func gen = ([], gen)
randomList len func gen = (elm:rest, gen'')
    where (elm, gen')   = func gen
          (rest, gen'') = randomList (len-1) func gen'

randomMatrix :: Int -> StdGen -> ([[Integer]], StdGen)
randomMatrix size gen = randomList size (randomList size randomElement) gen

main = do gen <- getStdGen 
          print $ randomMatrix 3 gen
