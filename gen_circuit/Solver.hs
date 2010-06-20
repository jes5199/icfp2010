-- Heuristic algorithms for solving cars.

import List
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

all_heuristics :: Monad m => [Heuristic m]
all_heuristics = [dumb_heuristic]

-- Toplevel "solve" tries all heuristics in order and returns the
-- result that serializes to the shortest sequence.
solve :: Monad m => Heuristic m
solve car = case sortBy compareLength successful_fuels
            of [] -> fail "All heuristics failed"
               ((f,_):_) -> return f
    where successful_fuels :: [(Fuel, Int)]
          successful_fuels = do heuristic <- all_heuristics
                                fuel <- heuristic car
                                check_fuel car fuel
                                return (fuel, length $ compileFuel fuel)
          compareLength (_,x) (_,y) = compare x y
