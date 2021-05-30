liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
  return (f i)

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = do
  a <- m1
  b <- m2
  return (f a b)

-- Log is an alias of [String]
type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) }

instance Monad Logger where
	return x = Logger (x, [])
        (>>=) m f = Logger (result, logs ++ log) where
          (v, logs) = execLogger m
          (result, log) = execLogger $ f v

instance Functor Logger where
  fmap f (Logger (v, logs)) = Logger (f v, logs)

instance Applicative Logger where
  pure = return
  Logger (f, la) <*> Logger (v, lb) = Logger (f v, la)

runLogger = execLogger

record :: String -> Logger ()
record string = Logger ((), [string])

-- Convert glob to regex
globToRegex :: String -> Logger String
globToRegex cs =
  globToRegexImpl cs >>= \ds ->
  return ('^': ds)

-- regex ends with `$`
globToRegexImpl :: String -> Logger String
globToRegexImpl [] = return "$"

-- 
globToRegexImpl ('?' : cs) =
  record "any" >>
  globToRegexImpl cs >>= \ds ->
  return ('.' : ds)

globToRegexImpl ('*' : cs) = do
  record "kleene star"
  ds <- globToRegexImpl cs
  return (".*" ++ ds)

globToRegexImpl ('[':'!':c:cs) =
  record "character class, negative" >>
  charClass cs >>= \ds ->
  return ("[^" ++ c : ds)

globToRegexImpl ('[':c:cs) =
  record "character class" >>
  charClass cs >>= \ds ->
  return ("[" ++ c : ds)

globToRegexImpl ('[':_) =
  return "unterminated character class"

globToRegexImpl (c:cs) = liftM2 (++) (escape c) (globToRegexImpl cs)

m = return "foo" :: Logger String
-- get the length of value wrapped by monad
len1 = m >>= \s -> return (length s)  -- 1. as Monad
len2 = pure length <*> m              -- 2. as Applicative

charClass (']' : cs) = (']':) `liftM` globToRegexImpl cs
charClass (c : cs) = (c:) `liftM` charClass cs

escape :: Char -> Logger String
escape c
  | c `elem` regexChars = record "escape" >> return ['\\', c]
  | otherwise           = return [c]
  where regexChars = "\\+()^$.{}[]|"

