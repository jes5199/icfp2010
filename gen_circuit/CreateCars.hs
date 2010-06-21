import Random
import Control.Monad
import System.IO.Unsafe
import CarParts
import FuelChecker
import NumberParser
import Solver
import List

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

chamberFactory :: Int -> Int -> Fuel -> [(Pipe,[[Integer]])] -> [ReactionChamber] -> StdGen -> ((Car, Fuel), StdGen)
chamberFactory 0 pipeLen fuel evaluated_pipes chambers gen = ((chambers, fuel), gen)
chamberFactory counter pipeLen fuel evaluated_pipes chambers gen | elem (pipe, matrix) evaluated_pipes = chamberFactory (counter-1) pipeLen fuel evaluated_pipes chambers gen'
                                                                 | otherwise = chamberFactory (counter-1) pipeLen fuel evaluated_pipes' final_chambers gen'
    where (pipe,gen')     = randomPipe pipeLen gen
          matrix          = eval_pipe pipe fuel
          new_chambers1   = concatMap (     makeValidReactionChambers (pipe,matrix) ) evaluated_pipes
          new_chambers2   = concatMap (flip makeValidReactionChambers (pipe,matrix) ) evaluated_pipes
          final_chambers  = new_chambers1 ++ new_chambers2 ++ chambers
          evaluated_pipes'= (pipe,matrix):evaluated_pipes

matrixLessThanOrEqual :: [[Integer]] -> [[Integer]] -> Bool
matrixLessThanOrEqual m1 m2 = all (\(a,b) -> a<=b ) $ zip (concat m1) (concat m2)

makeValidReactionChambers :: (Pipe, [[Integer]]) -> (Pipe, [[Integer]]) -> [ReactionChamber]
makeValidReactionChambers (p1, m1) (p2, m2) | not $ matrixLessThanOrEqual m1 m2 = []
                                            | otherwise = [(p2, flag, p1)]
                                                where flag = if m1 !! 0 !! 0 < m2 !! 0 !! 0
                                                             then 0
                                                             else 1

carFactory :: Int -> Int -> Int -> StdGen -> ((Car, Fuel), StdGen)
carFactory count pipelen ingredientCount gen = chamberFactory count pipelen fuel [] [] gen'
    where (fuel, gen') = randomFuel ingredientCount gen

normalizeCar :: Car -> String
normalizeCar car = minimum $ map carAsTrits allCars
    where allCars = map (flip permuteCar car) allPermutations 

carAsTrits :: Car -> String
carAsTrits car = (tritEncode $ toInteger $ length car) ++ (concat $ sort (map sectionAsTrit car))

sectionAsTrit (upper,flag,lower) = (compileList renderSchemeTwo upper) ++ (renderSchemeTwo flag) ++ (compileList renderSchemeTwo lower)

type Permutation = [Int]

permute :: [a] -> [[a]]
permute str = rotate str len len
   where len = length str

rotate :: [a] -> Int -> Int -> [[a]]
rotate _ _ 0 = []
rotate s 1 _ = [s]
rotate (ch:chs) len rcnt = 
   map (\x -> ch : x) (rotate chs (len-1) (len-1))
   ++ 
   rotate (chs ++ [ch]) len (rcnt-1)

allPermutations = permute [0,1,2,3,4,5]

permuteCar :: Permutation -> Car -> Car
permuteCar perm car = map permuteReactionChamber car
    where permuteReactionChamber (upper, flag, lower) = (map permuteSection upper, flag, map permuteSection lower)
          permuteSection n = toInteger $ perm !! fromInteger n

main = do gen <- getStdGen 
          let ((car, fuel), gen') = carFactory 11 10 4 gen
          putStrLn $ showCarAsEquations $ car
          putStrLn $ showFuelAsMatrices $ fuel
          print $ length car
          check_fuel car fuel
          print $ normalizeCar car
          print $ (solve car :: Maybe Fuel)
