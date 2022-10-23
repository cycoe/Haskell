{-# LANGUAGE Arrows #-}
module ArrowFunc where
import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))

-- arr :: (Arrow a) => (b -> c) -> a b c

newtype SimpleFunc a b = SimpleFunc
  { runF :: a -> b
  }

instance Arrow SimpleFunc where
  arr f = SimpleFunc f
  first (SimpleFunc f) = SimpleFunc (mapFst f) where
    mapFst g (a, b) = (g a, b)
  second (SimpleFunc f) = SimpleFunc (mapFst f) where
    mapFst g (a, b) = (a, g b)

instance Category SimpleFunc where
  (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
  id = arr id

-- split is an arrow value that splits a single value into a pair
-- of duplicate values
split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x, x))

-- unsplit is an arrow value that takes a pair of values and
-- combines them to return a single value:
unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry

-- (***) combines two arrow values by running them on a pair (the
-- first arrow value on the first component of the pair; the second
-- arrow value on the second component of the pair)
-- (***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
-- f *** g = first f >>> second g

-- (&&&) combines two arrow values by running them with the same input
-- (&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')
-- f &&& g = split >>> first f >>> second g

f :: SimpleFunc Int Int
f = arr (`div` 2)

g :: SimpleFunc Int Int
g = arr (\x -> x * 3 + 1)

h :: SimpleFunc Int Int
h = f &&& g >>> unsplit (+)

hOutput :: Int
hOutput = runF h 8

-- A lot of juggling occurred to get the plumbing right since h wasn't
-- defined as a linear combination of arrow values. GHC has a
-- syntactic notation that simplifies this in a similar way to how
-- do-notation simplifies monadic computations. The h function can
-- then be defined as
h' :: SimpleFunc Int Int
h' = proc x -> do
  fx <- f -< x
  gx <- g -< x
  returnA -< (fx + gx)

-- newtype Kleisli m a b = Kleisli {
--   runKleisli :: (a -> m b)
-- }

plusminus :: Kleisli [] Int Int
plusminus = Kleisli (\x -> [x, 0, -x])
double :: Kleisli [] Int Int
double = arr (*2)
h2 :: Kleisli [] Int (Int, Int)
h2 = plusminus &&& double

main :: IO ()
main = do
   let
       prepend x = arr (x ++)
       append  x = arr (++ x)
       withId  t = returnA <+> t
       xform = (withId $ prepend "<") >>>
               (withId $ append ">") >>>
               (withId $ ((prepend "!") >>> (append "!")))
       xs = ["test", "foobar"] >>= (runKleisli xform)
   mapM_ putStrLn xs
