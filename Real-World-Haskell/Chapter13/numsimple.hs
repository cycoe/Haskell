-- Supported operators
data Op = Plus | Minus | Mul | Div | Pow
  deriving (Eq, Show)

-- Core symbolic manipulation type
data SymbolicManip a =
  Number a |
  Symbol String |
  BinaryArith Op (SymbolicManip a) (SymbolicManip a) |
  UnaryArith String (SymbolicManip a)
  deriving (Eq)

-- SymbolicManip is an instance of Num
instance Num a => Num (SymbolicManip a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a b
  negate a = BinaryArith Mul (Number (-1)) a
  abs a = UnaryArith "abs" a
  signum _ = error "Unimplement"
  fromInteger i = Number (fromInteger i)

instance (Fractional a) => Fractional (SymbolicManip a) where
  a / b = BinaryArith Div a b
  recip a = BinaryArith Div (Number 1) a
  fromRational r = Number (fromRational r)

prettyShow :: (Show a, Num a) => SymbolicManip a -> String

prettyShow (Number x) = show x
prettyShow (Symbol x) = x

prettyShow (BinaryArith op a b) =
  let pa = handleParen a
      pb = handleParen b
      pop = padding . op2str $ op
      in pa ++ pop ++ pb

prettyShow (UnaryArith opstr a) =
  opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "^"

padding :: String -> String
padding str = " " ++ str ++ " "

handleParen :: (Show a, Num a) => SymbolicManip a -> String
handleParen (Number x) = prettyShow (Number x)
handleParen (Symbol x) = prettyShow (Symbol x)
handleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
handleParen x@(UnaryArith _ _) = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
  show a = prettyShow a
