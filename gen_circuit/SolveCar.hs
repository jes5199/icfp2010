-- Read a car in from standard input.  If it can be solved, solve it
-- and produce the resulting car on standard output.  If it can't,
-- produce no output and exit with error code 1.

import System
import Solver
import NumberParser

main = do input <- getContents
          case solve $ fst $ parseCar input
            of Nothing -> exitWith $ ExitFailure 1
               Just fuel -> putStrLn $ compileFuel fuel
