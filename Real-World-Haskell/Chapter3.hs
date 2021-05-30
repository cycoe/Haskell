-- Pattern Match
-- Match a complicated pattern
complicated (True, a, x:xs, 5) = (a, xs)
(a, b) = complicated (True, "Cycoe", [1, 2], 5)

-- Parameterised Types
-- Maybe is a parameterised type, and Maybe Int is a type, and
-- Just a is a constructor
-- Just :: a -> Maybe a
-- Nothing :: Maybe a
maybeString = Just "Cycoe"
maybeString2 = Just maybeString

-- Recursive Types
-- Our own list type
data List a = Cons a (List a)
            | Nil
	      deriving (Show)

-- Load data from build-in List
fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- Dump data to build-in List
toList :: List a -> [a]
toList (Cons x xs) = x : (toList xs)
toList Nil         = []

-- Our own tree type
data Tree a = Node a (Tree a) (Tree a)
            | Empty
	      deriving (Show)

-- Reporting errors
-- error :: String -> a
mdiv a b = if b == 0
	  then error "Cannot divided by zero!"
	  else a / b

-- We can also use Maybe to let caller to handle the error
safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
		else Just (head (tail xs))

-- A more tidy implement, notice the different order of parameters against above function
tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

-- Introducing local viriables (Bind expression to variable)
-- Notice 2 things:
-- 1. whitespcace in the expression
-- 2. haskell is lazy, newBalance will not be evalued util it's needed
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
		      in if balance < reserve
		         then Nothing
			 else Just newBalance

-- Another things is where
lend2 amount balance = if balance < reserve
                       then Nothing
		       else Just newBalance
	where reserve    = 100
	      newBalance = balance - amount

-- Define local function is similar with local variable
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
	  plural n = show n ++ " " ++ word ++ "s"
