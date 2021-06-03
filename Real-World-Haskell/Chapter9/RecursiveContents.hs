module RecursiveContents (getRecursiveContents) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents root = do
  -- Get directory names in path root
  names <- getDirectoryContents root
    -- Filtered "." and ".." for avoiding infinity recursive
  let filteredNames = filter (`notElem` [".", ".."]) names
  -- forM is another mapM
  -- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
  -- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
  paths <- forM filteredNames $ \name -> do
    let path = root </> name
    -- doesDirectoryExist :: FilePath -> IO Bool
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)
