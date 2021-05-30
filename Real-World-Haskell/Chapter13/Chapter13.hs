import Data.List(lookup)
import qualified Data.Map as Map

-- There are two types of Map
-- First one is Associate List
alist = [(1, "one"), (2, "two"), (3, "three")]

-- use lookup to lookup a value by a key
lookup1 = lookup 1 alist
lookup4 = lookup 4 alist

-- Map is more efficient implement of associate list
mapFromList = Map.fromList alist

-- Create a map by folding over a empty map
mapFromFold = foldl (\m (k, v) -> Map.insert k v m) Map.empty alist

------------ Function is also data -------------

-- Our own customColor type
data CustomColor =
  CustomColor
  { red :: Int
  , green :: Int
  , blue :: Int
  } deriving (Eq, Show, Read)

-- A new type that stores a name and a function. The function takes an
-- Int, applies some computation on it and returns an Int along with a
-- Customcolor.  That means that we can record a function in data
-- type, then use it any time.
data FuncRec =
  FuncRec
  { name :: String
  , colorCalc :: Int -> (CustomColor, Int)
  }

purple = CustomColor 255 0 255

plus5 = FuncRec {name = "plus5", colorCalc = \x -> (purple, x + 5)}
always0 = FuncRec {name = "always0", colorCalc = \_ -> (purple, 0)}

colorTuple1 :: (CustomColor, Int)
colorTuple1 = colorCalc plus5 2

colorTuple2 :: (CustomColor, Int)
colorTuple2 = colorCalc always0 2
