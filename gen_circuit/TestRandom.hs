import Data.IORef
import Random

type Foo = IORef StdGen

getRandomValue :: Random a => Foo -> IO a
getRandomValue foo = do gen <- readIORef foo
                        let (value, gen') = random gen
                        writeIORef foo gen'
                        return value

makeFoo :: IO Foo
makeFoo = do gen <- newStdGen
             newIORef gen



main = do foo <- makeFoo
          values <- sequence [(getRandomValue foo :: IO Int) | i <- [1..10]]
          print values
