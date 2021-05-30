import Text.ParserCombinators.Parsec

{- A CSV file contains 0 or more lines, each of which is terminated by the end-of-line charecter (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile = do
  result <- many line
  eof
  return result

line :: GenParser Char st [String]
line = do
  result <- cells
  eol
  return result

cells :: GenParser Char st [String]
cells = do
  first <- cellContent
  next <- remainCells
  return (first : next)

remainCells :: GenParser Char st [String]
remainCells =
  (char ',' >> cells)
    <|> return []

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknow)"
