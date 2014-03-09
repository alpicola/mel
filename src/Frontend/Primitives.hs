module Frontend.Primitives where

data PrimOp = Cmp CmpOp
            | Arith ArithOp
            | FArith FArithOp 
            deriving (Eq, Show)

data CmpOp = Eq | Neq | Lt | Le | Gt | Ge deriving Eq
data ArithOp = Plus | Minus | Times | Div deriving Eq
data FArithOp = FPlus | FMinus | FTimes | FDiv deriving Eq

instance Show CmpOp where
  show Eq = "="
  show Neq = "<>"
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="

instance Show ArithOp where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"

instance Show FArithOp where
  show FPlus = "+."
  show FMinus = "-."
  show FTimes = "*."
  show FDiv = "/."

evalCmpOp :: (Eq a, Ord a) => CmpOp -> [a] -> Bool
evalCmpOp Eq [x, y] = x == y
evalCmpOp Neq [x, y] = x /= y
evalCmpOp Lt [x, y] = x < y
evalCmpOp Le [x, y] = x <= y
evalCmpOp Gt [x, y] = x > y
evalCmpOp Ge [x, y] = x >= y

evalArithOp :: ArithOp -> [Int] -> Int
evalArithOp Minus [x] = -x
evalArithOp Plus [x, y] = x + y
evalArithOp Minus [x, y] = x - y
evalArithOp Times [x, y] = x * y
evalArithOp Div [x, y] = x `div` y

evalFArithOp :: FArithOp -> [Float] -> Float
evalFArithOp FMinus [x] = -x
evalFArithOp FPlus [x, y] = x + y
evalFArithOp FMinus [x, y] = x - y
evalFArithOp FTimes [x, y] = x * y
evalFArithOp FDiv [x, y] = x / y
