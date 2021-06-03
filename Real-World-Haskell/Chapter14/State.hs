import Control.Monad
import qualified System.Random as R

-- State monad is a function that transform a state of type `s` to
-- another and return a value with type `a`. Remember that State monad
-- is a function

-- Simple definition of State
-- type SimpleState s a = s -> (a, s)

-- Real definition of State
newtype State s a = State
  { runState :: s -> (a, s)
  }

-- But Monad has only one type argument, so here we bind `s` with String
type StringState = State String

instance Functor (State s) where
  fmap f m = State $ \s ->
    let (a, s') = runState m s
     in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) f m = State $ \s ->
    let (a, s') = runState m s
        (k, s'') = runState f s'
     in (k a, s'')

instance Monad (State s) where
  --(>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) m f = State $ \s ->
    let (a, s') = runState m s
     in runState (f a) s'

  -- Define the return function
  --return :: a -> State s a
  return a = State $ \s -> (a, s)

-- Read State
get :: State s s
get = State $ \s -> (s, s)

-- Put State
put :: s -> State s ()
put s = State $ const ((), s)

-- rand return an Int from range (0, 100)
rand :: IO Int
rand = R.getStdRandom (R.randomR (0, 100))

-- R.getStdRandom return random using StdGen of class RandomGen. We can difine our own random function of StdGen
twoBadRandoms :: R.RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ R.random gen, fst $ R.random gen)

-- We get the same value every time
badRandoms :: IO (Int, Int)
badRandoms = twoBadRandoms `fmap` R.getStdGen

-- A alt workaround is passing the gen through generation, we get different value in a and b
twoGoodRandoms :: R.RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen =
  let (a, gen') = R.random gen
      (b, gen'') = R.random gen'
   in ((a, b), gen'')

goodRandoms :: IO ((Int, Int), R.StdGen)
goodRandoms = fmap twoGoodRandoms R.getStdGen

-- Use StdGen as state
type RandomState a = State R.StdGen a

getRandom :: R.Random a => RandomState a
getRandom = do
  -- get 定义了一种特殊的状态转移，保持状态 s 不变并将 s 作为值返回
  gen <- get
  -- 使用 get 到的生成器状态生成随机数 val 和新的状态 gen'
  let (val, gen') = R.random gen
  -- put 定义了一种特殊的状态转移，使用 gen' 代替了传入的状态
  put gen'
  -- 返回包含 val 的状态转移
  return val

getTwoRandoms :: (R.Random a, R.Random b) => State R.StdGen (a, b)
getTwoRandoms = liftM2 (,) getRandom getRandom

veryGoodRandoms :: IO ((Int, Int), R.StdGen)
veryGoodRandoms = fmap (runState getTwoRandoms) R.getStdGen

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- R.getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  R.setStdGen newState
  return result

-- Define a data structure to handle more state
data CountedRandom = CountedRandom
  { crGen :: R.StdGen,
    crCount :: Int
  }
  deriving (Show)

type CRState = State CountedRandom

getCountedRandom :: R.Random a => CRState a
getCountedRandom = do
  state <- get
  let (val, gen) = R.random (crGen state)
  put CountedRandom {crGen = gen, crCount = crCount state + 1}
  return val

-- 构造获取两个随机数的 CountedRandom 单子
getTwoCountedRandoms :: (R.Random a, R.Random b) => State CountedRandom (a, b)
getTwoCountedRandoms = liftM2 (,) getCountedRandom getCountedRandom

-- 使用一个工具函数通过 R.StdGen 构造一个 CountedRandom 单子
constructCR :: R.StdGen -> CountedRandom
constructCR gen = CountedRandom {crGen = gen, crCount = 0}

-- 运行 getTwoCountedRandoms
twoCountedRandoms :: (R.Random a, R.Random b) => IO ((a, b), CountedRandom)
twoCountedRandoms =
  -- 使用状态转移函数映射由 IO 包裹的随机数生成器
  runState getTwoCountedRandoms `fmap` gen where
  -- 使用构造函数映射由 IO 包裹的随机数生成器
  gen = constructCR `fmap` R.getStdGen
