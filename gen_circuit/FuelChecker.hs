module FuelChecker where

import List
import CarParts
import Matrices

-- Check fuel either succeeds (returning ()) or errors (giving a
-- descriptive message) depending on whether the car and fuel match.
check_fuel :: Monad m => Car -> Fuel -> m ()
check_fuel car fuel = do check_enough_tanks car fuel
                         sequence_ [check_chamber i chamber fuel | (chamber, i) <- zip car [0..]]

check_enough_tanks :: Monad m => Car -> Fuel -> m ()
check_enough_tanks car fuel = check (n <= genericLength fuel) msg
    where n = num_required_tanks car
          msg = "Car requires " ++ show n ++ " tanks, but only " ++ show (length fuel) ++ " present"

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

show_chamber_results :: ReactionChamber -> Fuel -> String
show_chamber_results (upper, flag, lower) fuel
    = unlines $
      [lhs ++ " " ++ comparator ++ " " ++ rhs ++ "  (diff: " ++ diff ++ ")"
       | (lhs, comparator, rhs, diff)
           <- zip4 (showMatrixLines upper_matrix) (makeComparator flag) (showMatrixLines lower_matrix)
              (showMatrixLines diff_matrix)]
    where upper_matrix = eval_pipe upper fuel
          lower_matrix = eval_pipe lower fuel
          diff_matrix = zipWith (zipWith (-)) upper_matrix lower_matrix
          makeComparator 0 = "> " : makeComparator 1
          makeComparator _ = repeat ">="

manual_test_chamber :: Car -> Int -> Fuel -> IO ()
manual_test_chamber car chamber_num fuel = putStr $ show_chamber_results (car !! chamber_num) fuel
