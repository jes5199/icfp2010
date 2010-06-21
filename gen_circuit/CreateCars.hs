import Random
import Control.Monad
import System.IO.Unsafe
import CarParts

randomElement :: StdGen -> (Integer, StdGen)
randomElement gen = randomR (0,1) gen

randomList :: Int -> (StdGen -> (a, StdGen)) -> StdGen -> ([a], StdGen)
randomList 0   func gen = ([], gen)
randomList len func gen = (elm:rest, gen'')
    where (elm, gen')   = func gen
          (rest, gen'') = randomList (len-1) func gen'

randomMatrix :: Int -> StdGen -> ([[Integer]], StdGen)
randomMatrix size gen = randomList size (randomList size randomElement) gen

randomFuelComponent :: Int -> StdGen -> ([[Integer]], StdGen)
randomFuelComponent size gen = if matrix !! 0 !! 0 /= 0
                               then (matrix, gen')
                               else randomFuelComponent size gen'
    where (matrix, gen') = randomMatrix size gen 

randomFuel :: Int -> StdGen -> (Fuel, StdGen)
randomFuel ingredientCount gen = randomList 6 (randomFuelComponent ingredientCount) gen

main = do gen <- getStdGen 
          print $ randomFuel 3 gen
