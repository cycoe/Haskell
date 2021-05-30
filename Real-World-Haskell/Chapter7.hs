--- IO

import System.IO
import Data.Char(toUpper)

-- A function return IO Monad
-- Attention to the type of putStrLn and getLine
-- putStrLn :: String -> IO ()
-- getLine :: IO String
whatIsYourName :: IO ()
whatIsYourName = do
  putStrLn "Greeting! What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"


-- A more punk implement
-- Attention to the type of >> and >>=
-- (>>) :: Monad m => m a -> m b -> m b
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
whatIsYourName' :: IO ()
whatIsYourName' =
  putStrLn "Greeting! What is your name?" >>
  getLine >>=
  \name -> putStrLn $ "Hello, " ++ name ++ "!"


-- let Alphabet to upper from inh to outh
fileToUpper :: Handle -> Handle -> IO ()
fileToUpper inh outh = do
  ineof <- hIsEOF inh
  if ineof
  -- This return means pack () tuple into IO Monad
  then return ()
  else do
    inStr <- hGetLine inh
    hPutStrLn outh (map toUpper inStr)
    fileToUpper inh outh

-- Process inFile with process and output into outFile
fileProcess :: String -> String -> (Handle -> Handle -> IO ()) -> IO ()
fileProcess inFile outFile process = do
  inh <- openFile inFile ReadMode
  outh <- openFile outFile WriteMode
  process inh outh
  hClose inh
  hClose outh

-- A more elegant fileToUpper sample and extended to more process method
-- hGetContents :: Handle -> IO String
fileToUpper' :: Handle -> Handle -> IO ()
fileToUpper' inh outh = do
  inStr <- hGetContents inh
  hPutStr outh $ map toUpper inStr

-- A more elegant fileProcess
-- readFile :: FilePath -> IO String
-- writeFile :: FilePath -> String -> IO ()
fileProcess' :: FilePath -> FilePath -> (String -> String) -> IO ()
fileProcess' inFile outFile process = do
  inStr <- readFile inFile
  writeFile outFile $ process inStr

main = do
  whatIsYourName
