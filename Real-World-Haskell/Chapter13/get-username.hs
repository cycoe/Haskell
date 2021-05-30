import Data.List
import System.IO
import Control.Monad(when)
import System.Exit
import System.Environment(getArgs)

main = do
  -- read command line arguments
  args <- getArgs

  -- If we don't have the right amount of args, give and error and quit
  when (length args /= 2) $ do
    putStrLn "Syntax: get-username filename uid"
    exitFailure

  -- read file lazily
  content <- readFile (args !! 0)

  let username = findByUID content (read (args !! 1))

  -- Display the result
  case username of
    Just x  -> putStrLn x
    Nothing -> putStrLn "Cannot find UID"

-- find UID by in given content
findByUID :: String -> Integer -> Maybe String
findByUID content uid =
  let a1 = map parseLine . lines $ content
      in lookup uid a1

parseLine :: String -> (Integer, String)
parseLine input =
  let fields = split ':' input
      in (read (fields !! 2), fields !! 0)

--
split _ [] = [[]]
split delim string =
  let (before, remainder) = span (/= delim) string
      in before : case remainder of
                    [] -> []
                    x  -> split delim (tail x)
