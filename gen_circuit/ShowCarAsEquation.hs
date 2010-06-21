import NumberParser

main = interact ((++"\n") . showCarAsEquations . fst . parseCar)
