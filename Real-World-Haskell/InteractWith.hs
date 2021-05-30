-- In most of this chapter, we will concern ourselves with code that has no interaction with the outside world. To maintain our focus on practical code, we will begin by developing a gateway between our “pure” code and the outside world. Our framework simply reads the contents of one file, applies a function to the file, and writes the result to another file. 

import qualified System.Environment as Env

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- Env.getArgs
          case args of
            [input, output] -> interactWith function input output
            _               -> putStrLn "error: exactly two arguments needed!"

        -- replace "id" with the name of our function below
	myFunction = id
