import NumberParser


showCars :: String -> String
showCars input = unlines $ map showCarAndName $ lines input 
    where showCarAndName carString = carString ++ "\n" ++ (prettyCar car) ++ "remaining: " ++ show remaining ++ "\n"
           where (car, remaining) = parseCar carString

main = interact showCars
