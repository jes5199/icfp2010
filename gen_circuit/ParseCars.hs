import NumberParser


showCars :: String -> String
showCars input = unlines $ map showCarAndName $ lines input 
    where showCarAndName carString = carString ++ "\n" ++ (prettyCar car) ++ showCarAsEquations car ++ "\nremaining: "
                                     ++ show remaining ++ "\n"
           where (car, remaining) = parseCar carString

main = interact showCars
