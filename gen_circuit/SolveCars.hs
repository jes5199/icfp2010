import NumberParser
import CarParts
import Solver

main = interact solveCars

solveCars :: String -> String
solveCars input = unlines $ map showCarAndSolution $ lines input

showCarAndSolution :: String -> String
showCarAndSolution encoded_car = encoded_car ++ "\n" ++ showCarAsEquations car ++ "\n" ++ showSolution (solve car) ++ "\n\n"
    where car = fst $ parseCar encoded_car
          showSolution Nothing = "No solution found"
          showSolution (Just fuel) = "Successfully found a solution:\n" ++ showFuelAsMatrices fuel
