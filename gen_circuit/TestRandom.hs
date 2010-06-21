import Data.IORef
import Random

getRandomValue :: Random a => IO a
getRandomValue = do gen <- newStdGen
                    return (fst $ random gen)



main = do values <- sequence [(getRandomValue :: IO Int) | i <- [1..10]]
          print values
