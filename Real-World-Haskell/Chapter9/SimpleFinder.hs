-- Enable extension for type signature for lambda
{-# LANGUAGE ScopedTypeVariables #-}

import RecursiveContents (getRecursiveContents)
import System.FilePath (takeExtension, (</>))
import System.Directory (getDirectoryContents, getPermissions, Permissions(..), getModificationTime, doesFileExist)
import System.IO (openFile, IOMode(ReadMode), hClose, hFileSize)
import Data.Time.Clock (UTCTime(..))
import Control.Monad (forM, mapM, liftM, filterM)
import Control.Exception (bracket, handle, SomeException(..))

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  -- Use prediction p to filter names
  return $ filter p names

haskellFind :: FilePath -> IO [FilePath]
-- haskellFind use extension compare to filter ".hs" source file
haskellFind = simpleFind (\p -> takeExtension p == ".hs")

-- Our simpleFind has 3 problems
-- 1. Cannot tell the difference between directory and file with pattern
-- 2. No method to exclude directories
-- 3. simpleFind is strict that we return all paths once evaled

-- Keep the purity of prediction while from plain type to rich type

-- A sample of plain type prediction
plainPredict :: FilePath -> Bool
plainPredict name = takeExtension name == ".hs"

-- We can use this prediction to find files
haskellFiles :: IO [FilePath]
haskellFiles = simpleFind plainPredict "."

-- To extend the function of prediction, we could use IO (Bool) to get informations with IO State
badRichFind :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
badRichFind p path = do
  names <- getRecursiveContents path
  -- Use prediction p to filter names
  filterM p names

-- A good usage of badRichFind
someFiles :: IO [FilePath]
someFiles = badRichFind doesFileExist "."

-- But we can also do some bad things over it
badSomeFiles :: IO [FilePath]
badSomeFiles = flip badRichFind "." $ \name -> do
  -- We could do anything over IO here, so we should keep prediction pure
  writeFile "badSideEffect.txt" "Bad thing happened!"
  doesFileExist name

-- getPermissions let us know more about directory permissions
directoryPerm :: IO Permissions
directoryPerm = getPermissions "."

-- getModificationTime
directoryMT :: IO UTCTime
directoryMT = getModificationTime "."

-- Define a rich prediction
type Prediction = FilePath      -- path
               -> Permissions   -- permissions
               -> Maybe Integer -- file size
               -> UTCTime       -- last modified
               -> Bool          --

betterFind :: Prediction -> FilePath -> IO [FilePath]
betterFind p path = do
  names <- getRecursiveContents path
  filterM check names where
    -- check is a prediction wrapper with IO
    -- check :: FilePath -> IO Bool
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return (p name perms size modified)

-- We can get size of a file with System.IO
-- If file does not exist, it will throw out an exception
simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  handle <- openFile path ReadMode
  size <- hFileSize handle
  hClose handle
  return size

-- A safer implement of fileSize function with handle 
saferFileSize :: FilePath -> IO (Maybe Integer)
-- handle the exception
saferFileSize path = handle error $ do
  fp <- openFile path ReadMode
  size <- hFileSize fp
  hClose fp
  return (Just size) where
    -- This signature is needed that deduce a specific Exception
    error :: SomeException -> IO (Maybe Integer)
    error _ = return Nothing

-- Find files that size is smaller than 1KB and with suffix ".hs"
hsSrcPred :: Prediction
hsSrcPred name _ size _ =
  takeExtension name == ".hs" &&
  case size of Just s  -> s < 1024
               Nothing -> False

-- saferFileSize seams much safer, but it still cannot work properly in all situations. If we openFile successfully, but hFileSize failed, we will lose a hClose. So we want to handle the open-action-close cycle

-- Here is bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c

-- The first action we get a resource, the second action we release a resource, the third action called bewteen them

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: SomeException) -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
  size <- hFileSize h
  return (Just size)

-------------- DSL --------------
type InfoP a = FilePath
            -> Permissions
            -> Maybe Integer
            -> UTCTime
            -> a

-- extract path from prediction
pathP :: InfoP FilePath
pathP path _ _ _ = path

-- extract size
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
-- we use -1 as Nothing here
sizeP _ _ Nothing     _ = -1

-- equalP construct a prediction
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

-- We can define more words here, with lifting
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP cmp getter args w x y z = getter w x y z `cmp` args

-- Define greaterP and lesserP
greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

-- Prediction combinator
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 com p q w x y z = p w x y z `com` q w x y z

-- Define combinations
andP = liftP2 (&&)
orP = liftP2 (||)

-- lift path
liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

--
hsSrcPred' = (liftPath takeExtension `equalP` ".hs") `andP` (sizeP `greaterP` 1024)

-- Definde words as operators () is needed here for keep order
(==?) = equalP
(>?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?)  = greaterP
(<?)  = lesserP
(&&?) = andP
(||?) = orP

orgSrcPred = (liftPath takeExtension ==? ".org") &&? (sizeP <? 4096)
