import Control.Applicative
import GHC.Unicode (isDigit)

-- JsonValue

data JsonValue
  = JsonNull
  | JsonBool   Bool
  | JsonNumber Int
  | JsonString String
  | JsonArray  [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Eq, Show)

newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', c) <- p input
    Just (input', f c)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser a = Parser $ \input -> do
    (input', f') <- f input
    (input'', a') <- a input'
    return (input'', f' a')
  Parser a <* Parser b =
    fst <$> ((,) <$> (Parser a) <*> (Parser b))
  Parser a *> Parser b =
    snd <$> ((,) <$> (Parser a) <*> (Parser b))

instance Alternative Parser where
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input
  empty = Parser $ \input -> Nothing

charP :: Char -> Parser Char
charP c = Parser $ \input -> case input of
  (x:xs) | c == x -> Just (xs, c)
  _               -> Nothing

-- Next we should implement a stringP
-- we notice that if we want to match a string, we match some chars in sequence
-- map charP "null" :: [Parser Char]
-- we want to transform [Parser Char] into Parser [Char]
-- we can use sequenceA ::(Traversable t, Applicative f) => t (f a) -> f (t a)
-- we need to implement Parser as an instance of Applicative

mySequence :: (Applicative f) => [f a] -> f [a]
mySequence (f:fs) = (:) <$> f <*> mySequence fs
mySequence _      = pure []

stringP :: String -> Parser String
stringP s = mySequence $ charP <$> s

spanP :: (Char -> Bool) -> Parser String
spanP p = Parser $ \input ->
  let (token, rest) = span p input in
    Just (rest, token)

notNullP :: Parser [a] -> Parser [a]
notNullP (Parser p) = Parser $ \input -> do
  (input', x) <- p input
  case x of
    [] -> Nothing
    _  -> return (input', x)

literalP :: Parser String
literalP = Parser $ \input ->
  let (token, rest) = span (/='"') input
  in return (rest, token)

jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = construct <$> (stringP "true" <|> stringP "false") where
  construct "true"  = JsonBool True
  construct "false" = JsonBool False
  construct _       = undefined

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNullP (spanP isDigit)

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> literalP <* charP '"')

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString

main :: IO ()
main = undefined
