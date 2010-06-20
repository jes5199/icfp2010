module FuelChecker where

import List
import CarParts

-- Check fuel either succeeds (returning ()) or errors (giving a
-- descriptive message) depending on whether the car and fuel match.
check_fuel :: Monad m => Car -> Fuel -> m ()
check_fuel car fuel = do check_enough_tanks car fuel
                         sequence_ [check_chamber i chamber fuel | (chamber, i) <- zip car [0..]]

check_enough_tanks :: Monad m => Car -> Fuel -> m ()
check_enough_tanks car fuel = check (num_required_tanks <= genericLength fuel) msg
    where num_required_tanks = max0 [max (max0plus1 upper) (max0plus1 lower) | (upper, flag, lower) <- car]
          msg = "Car requires " ++ show num_required_tanks ++ " tanks, but only " ++ show (length fuel) ++ " present"
          max0 xs = maximum (0:xs)
          max0plus1 = max0 . map (1+)

multiply_matrices :: [[Integer]] -> [[Integer]] -> [[Integer]]
multiply_matrices a b = [rowTimesTransposedMatrix r b' | r <- a]
    where b' = transpose b

rowTimesTransposedMatrix :: [Integer] -> [[Integer]] -> [Integer]
rowTimesTransposedMatrix r b' = [sum $ zipWith (*) r c | c <- b']

identity_matrix :: Int -> [[Integer]]
identity_matrix n = [[if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n]]

num_ingredients :: Fuel -> Int
num_ingredients = length . head

eval_pipe :: Pipe -> Fuel -> [[Integer]]
eval_pipe pipe fuel = foldr multiply_matrices (identity_matrix $ num_ingredients fuel)
                      $ map ((fuel !!) . fromInteger) pipe

-- This check is predicated on the logic that for main chambers, the
-- difference between the product matrices must be >0 for all
-- components on the top row.
check_chamber :: Monad m => Int -> ReactionChamber -> Fuel -> m ()
check_chamber chamber_num (upper, flag, lower) fuel
    = sequence_ [sequence_ [check (diff >= threshold) (msg i j diff threshold)
                            | (j, diff) <- zip [1..] diff_row]
                 | (i, diff_row, threshold) <- zip3 [1..] matrix_difference thresholds]
    where upper_matrix = eval_pipe upper fuel
          lower_matrix = eval_pipe lower fuel
          matrix_difference = zipWith (zipWith (-)) upper_matrix lower_matrix
          thresholds | flag == 0 = 1 : replicate (n-1) 0
                     | otherwise = replicate n 0
          n = num_ingredients fuel
          msg i j diff threshold = "in chamber " ++ show chamber_num ++ ", upper_" ++ show i ++ "_" ++ show j ++
                                   " - lower_" ++ show i ++ "_" ++ show j ++ " = " ++ show diff ++ ", must be >= " ++
                                   show threshold

-- If the condition is true, do nothing.  Otherwise fail with the
-- given message.
check :: Monad m => Bool -> String -> m ()
check True _ = return ()
check False s = fail s
