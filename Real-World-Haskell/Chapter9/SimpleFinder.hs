import RecursiveContents (getRecursiveContents)
import System.FilePath (takeExtension)
import System.Directory (getPermissions, Permissions(..), getModificationTime, doesFileExist)
import Data.Time.Clock (UTCTime(..))
import Control.Monad (filterM)

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

--
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize name = return $ Just 10

betterFind :: Prediction -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check where
  check name = do
    perms <- getPermissions name
    size <- getFileSize name
    modified <- getModificationTime name
    return (p name perms size modified)
