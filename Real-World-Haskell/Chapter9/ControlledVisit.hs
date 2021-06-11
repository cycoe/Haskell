-- Enable extension for type signature for lambda
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (traverse)
import Data.List (sort)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension, (</>))
import System.Directory (getDirectoryContents, getPermissions, Permissions(..), getModificationTime, doesFileExist)
import System.IO (openFile, IOMode(ReadMode), hClose, hFileSize)
import Control.Exception (bracket, handle, SomeException(..))
import Control.Monad (forM, mapM, liftM, filterM)


-- Use a ADT to handle the predicate
data Info = Info
  { infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

-- Define a function to traverse
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  -- get names in current path
  names <- getUsefulContents path
  -- get info of current path and sub items
  contents <- mapM getInfo (path : map (path </>) names)
  -- for item in contents, we first filter them with `order`. Then check if it's a directory, if true, we handle with the sub directory, else we return a singleton with item. Last, we concat the 2-order list into an 1-order list
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

-- getUsefulContents is simple defined, just list a path and filtered with [".", ".."]
getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

-- Notice the type of maybe function
-- maybe :: b -> (a -> b) -> Maybe a -> b
isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(_ :: SomeException) -> return Nothing) (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

-- Traverse directories with sort
listedNames :: IO [FilePath]
listedNames = do
  infos <- traverse sort "."
  return $ map infoPath infos

-- Question 1. traverse with reverse alphabetic
reversedNames :: IO [FilePath]
reversedNames = do
  infos <- traverse (reverse . sort) "."
  return $ map infoPath infos

-- Question 2. write an order function to implement LRD
lrdNames :: IO [FilePath]
lrdNames = do
  infos <- traverse (\l -> tail l ++ [head l]) "."
  return $ map infoPath infos
