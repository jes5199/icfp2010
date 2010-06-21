import Random
import Control.Monad
import System.IO.Unsafe
import CarParts
import FuelChecker

randomList :: Int -> (StdGen -> (a, StdGen)) -> StdGen -> ([a], StdGen)
randomList 0   func gen = ([], gen)
randomList len func gen = (elm:rest, gen'')
    where (elm, gen')   = func gen
          (rest, gen'') = randomList (len-1) func gen'

randomMatrix :: Int -> StdGen -> ([[Integer]], StdGen)
randomMatrix size gen = randomList size (randomList size $ randomR (0,1) ) gen

randomFuelComponent :: Int -> StdGen -> ([[Integer]], StdGen)
randomFuelComponent size gen = if matrix !! 0 !! 0 /= 0
                               then (matrix, gen')
                               else randomFuelComponent size gen'
    where (matrix, gen') = randomMatrix size gen 

randomFuel :: Int -> StdGen -> (Fuel, StdGen)
randomFuel ingredientCount gen = randomList 6 (randomFuelComponent ingredientCount) gen

randomPipe :: Int -> StdGen -> (Pipe, StdGen)
randomPipe len = randomList len $ randomR (0,5)

carFactory :: Int -> Int -> Fuel -> [(Pipe,[[Integer]])] -> [ReactionChamber] -> StdGen -> ((Car, Fuel), StdGen)
carFactory 0 pipeLen fuel evaluated_pipes chambers gen = ((chambers, fuel), gen)
carFactory counter pipeLen fuel evaluated_pipes chambers gen = carFactory (counter-1) pipeLen fuel evaluted_pipes' final_chambers gen'
    where (pipe,gen')     = randomPipe pipeLen gen
          matrix          = eval_pipe pipe fuel
          new_chambers1   = concatMap (     makeValidReactionChambers (pipe,matrix) ) evaluated_pipes
          new_chambers2   = concatMap (flip makeValidReactionChambers (pipe,matrix) ) evaluated_pipes
          final_chambers  = new_chambrers1 ++ new_chambers2 ++ chambers
          evaluated_pipes'= (pipe,matrix):evaluated_pipes

matrixLessThanOrEqual :: [[Integer]] -> [[Integer]] -> Bool
matrixLessThanOrEqual m1 m2 = all [ all [ a <= b | (a,b) <- zip row1 row2 ] | (row1,row2) <- zip m1 m2 ]

makeValidReactionChambers :: (Pipe, [[Integer]]) -> (Pipe, [[Integer]]) -> [ReactionChamber]
makeValidReactionChambers (p1, m1) (p2, m2) | not $ matrixLessThanOrEqual m1 m2 = []
                                            | otherwise = [(p2, flag, p1)]
                                                where flag = if m1 !! 0 !! 0 < m2 !! 0 !! 0
                                                             then 0
                                                             else 1

main = do gen <- getStdGen 
          print $ randomPipe 10 gen
