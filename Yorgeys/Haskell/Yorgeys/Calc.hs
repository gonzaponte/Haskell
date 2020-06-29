module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x  ) =      x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

maybeEval :: Maybe ExprT -> Maybe Integer
maybeEval Nothing  = Nothing
maybeEval (Just x) = Just (eval x)

evalStr :: String -> Maybe Integer
evalStr = maybeEval .  parseExp Lit Add Mul


class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)


instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)


instance Expr Bool where
  lit = (LT ==) . compare 0
  add = (||)
  mul = (&&)


instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
