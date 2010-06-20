import NumberParser
import CarParts
import Solver

main = interact solveCars

solveCars :: String -> String
solveCars input = unlines $ map (showCarAndSolution . fst . parseCar) $ lines input

showCarAndSolution :: Car -> String
showCarAndSolution car = showCarAsEquations car ++ "\n" ++ showSolution (solve car) ++ "\n\n"
    where showSolution Nothing = "No solution found"
          showSolution (Just fuel) = "Successfully found a solution:\n" ++ showFuelAsMatrices fuel
