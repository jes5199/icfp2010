module Solver where

-- Heuristic algorithms for solving cars.

import List
import Maybe
import Ratio
import CarParts
import NumberParser
import FuelChecker

-- A heuristic is a monadic procedure that takes a car as input and
-- either yields a fuel or fails.  In general it is not necessary to
-- check whether the solution is correct--the top-level "solve"
-- function will take care of that.  Each heuristic function should
-- have type "Monad m => Heuristic m".
type Heuristic m = Car -> m Fuel

-- "dumb" heuristic just tries the fuel [[[2]]], which will work in
-- some cars.
dumb_heuristic :: Monad m => Heuristic m
dumb_heuristic _ = return [[[2]]]

-- Heuristic that tries all fuels that are 1x1 matrices containing
-- values equal to 1 or 2.
simple_1x1_heuristic :: Monad m => Heuristic m
simple_1x1_heuristic car = find_shortest [\_ -> return (make_fuel values) | values <- cartesian_product n [1,2]]
                           "No simple 1x1 fuels work" car
    where n = num_required_tanks car
          make_fuel :: [Integer] -> Fuel
          make_fuel values = [[[x]] | x <- values]

cartesian_product :: Integer -> [a] -> [[a]]
cartesian_product 0 _ = [[]]
cartesian_product i xs = do x <- xs
                            ys <- cartesian_product (i-1) xs
                            return (x : ys)

-- Heuristic that tries all combinations of the matrices
-- [1 0] [1 1] [2 1] [1 1]
-- [1 1] [0 1] [1 1] [1 1]
simple_2x2_heuristic :: Monad m => Heuristic m
simple_2x2_heuristic car = find_shortest [(\_ -> return matrices) | matrices <- cartesian_product n choices]
                           "No simple 2x2 fuels work" car
    where n = num_required_tanks car
          choices = [[[1,0],[1,1]],[[1,1],[0,1]],[[2,1],[1,1]],[[1,1],[1,1]]]

data InequalityType = Greater | GreaterOrEq
    deriving Eq

-- Heuristic that solves cases like this:
-- A > B^6  ==>  A = [33]
-- B^8 > A       B = [2]
generalized_1x1_heuristic :: Monad m => Heuristic m
generalized_1x1_heuristic car = iterate 0 (genericReplicate num_tanks 1)
    where num_tanks = num_required_tanks car
          iterate n values
              | n > 100 = fail "gave up after 100 iterations"
              | otherwise = case find_first_unsatisfied_inequality values
                             of Nothing -> return [[[x]] | x <- values]
                                Just (exps,typ)
                                    -> do let (i, largest_exp) = findLargest exps
                                              values_with_value_i_set_to_1 = setElem i 1 values
                                              frac = eval_lhs exps values_with_value_i_set_to_1
                                              inverted_frac = 1/frac
                                              min_value_to_exp = case typ
                                                                 of Greater -> floor inverted_frac + 1
                                                                    _ -> ceiling inverted_frac
                                          if largest_exp < 1 then fail "largest_exp < 1" else return ()
                                          if min_value_to_exp > limit then fail "numbers too big" else return ()
                                          let new_value = ceil_int_root largest_exp min_value_to_exp
                                          if new_value > 2^1024 then fail "values too large" else return ()
                                          iterate (n+1) (setElem i new_value values)
          inequalities :: [([Integer], InequalityType)]
          inequalities = [([countOccurrences i upper - countOccurrences i lower | i <- [0..(num_tanks-1)]],
                           if flag == 0 then Greater else GreaterOrEq
                          ) | (upper, flag, lower) <- car]
          eval_lhs :: [Integer] -> [Integer] -> Rational
          eval_lhs exps values = product $ zipWith valueToExp values exps
          valueToExp :: Integer -> Integer -> Rational
          valueToExp x y | y >= 0 = (x % 1) ^ y
                         | otherwise = (1 % x) ^ (-y)
          limit = (2^1024)^1024
          find_first_unsatisfied_inequality values = find (is_inequality_unsatisfied values) inequalities
          is_inequality_unsatisfied values (exponents, typ) | lhs < 1 = True
                                                            | (lhs == 1) = (typ == Greater)
                                                            | otherwise = False
              where lhs = eval_lhs exponents values

countOccurrences :: Eq a => a -> [a] -> Integer
countOccurrences n = genericLength . filter (n==)

-- Compute the minimum x such that x^root >= value
ceil_int_root :: Integer -> Integer -> Integer
ceil_int_root 1 value = value
ceil_int_root root value | value <= 0 = 0
                         | value == 1 = 1
                         | otherwise = uncurry binarySearch $ makeInitialGuess
    where makeGuessSequence n = n : makeGuessSequence (2*n)
          guessSequence = makeGuessSequence 2 -- [2,4,8,16,...]
          makeInitialGuess = let hi = fromJust $ find (\x -> x^root >= value) guessSequence
                             in (hi `div` 2, hi)
          binarySearch lo hi | lo + 1 >= hi = hi
                             | mid^root >= value = binarySearch lo mid
                             | otherwise = binarySearch mid hi
              where mid = (lo + hi) `div` 2

setElem :: Int -> a -> [a] -> [a]
setElem 0 x (y:ys) = x:ys
setElem n x (y:ys) = y : setElem (n-1) x ys

findLargest :: Ord a => [a] -> (Int, a)
findLargest xs = (\(x,y) -> (y,x)) $ maximum $ zip xs [0..]

all_heuristics :: Monad m => [Heuristic m]
all_heuristics = [dumb_heuristic, simple_1x1_heuristic, simple_2x2_heuristic, generalized_1x1_heuristic]

-- Run a list of heuristics and return the result that serializes to
-- the shortest sequence.
find_shortest :: (Monad m) => [Heuristic []] -> String -> Heuristic m
find_shortest heuristics fail_msg car = case sortBy compareLength successful_fuels
                                        of [] -> fail fail_msg
                                           ((f,_):_) -> return f
    where successful_fuels :: [(Fuel, Int)]
          successful_fuels = do heuristic <- heuristics
                                fuel <- heuristic car
                                check_fuel car fuel
                                return (fuel, length $ compileFuel fuel)
          compareLength (_,x) (_,y) = compare x y

-- Toplevel "solve" tries all heuristics in order and returns the
-- result that serializes to the shortest sequence.
solve :: Monad m => Heuristic m
solve = find_shortest all_heuristics "All heuristics failed"
