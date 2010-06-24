-- Send the desired fuel trinary stream to standard input (just digits).  Any character that isn't 0-2 is ignored.
-- Standard out is a circuit that produces that fuel, with prefix included

import Circuitry
import Simulator
import GenCircuit

main = interact ((++"\n") . compileCircuit . parseInput)

parseInput :: String -> [Trit]
parseInput = concatMap parseChar
    where parseChar '0' = [0]
          parseChar '1' = [1]
          parseChar '2' = [2]
          parseChar _ = []
