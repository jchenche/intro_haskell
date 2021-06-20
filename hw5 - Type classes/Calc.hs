{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where
import ExprT
import Parser
import StackVM
import qualified Data.Map as M

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit e)     = e
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr string =
    case parseExp ExprT.Lit ExprT.Add ExprT.Mul string of
        (Just e) -> Just (eval e)
        Nothing  -> Nothing

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit e     = ExprT.Lit e
    add e1 e2 = ExprT.Add e1 e2
    mul e1 e2 = ExprT.Mul e1 e2

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit e     = e
    add e1 e2 = e1 + e2
    mul e1 e2 = e1 * e2

instance Expr Bool where
    lit e     = e > 0
    add e1 e2 = e1 || e2
    mul e1 e2 = e1 && e2

instance Expr MinMax where
    lit e                       = MinMax e
    add (MinMax e1) (MinMax e2) = MinMax (max e1 e2)
    mul (MinMax e1) (MinMax e2) = MinMax (min e1 e2)

instance Expr Mod7 where
    lit e                   = Mod7 (e `mod` 7)
    add (Mod7 e1) (Mod7 e2) = Mod7 ((e1 + e2) `mod` 7)
    mul (Mod7 e1) (Mod7 e2) = Mod7 ((e1 * e2) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 5
instance Expr Program where
    lit e     = [PushI e]
    add e1 e2 = e1 ++ e2 ++ [StackVM.Add]
    mul e1 e2 = e1 ++ e2 ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = LitAlt Integer
              | AddAlt VarExprT VarExprT
              | MulAlt VarExprT VarExprT
              | VarAlt String
              deriving (Show, Eq)

instance Expr VarExprT where
    lit e     = LitAlt e
    add e1 e2 = AddAlt e1 e2
    mul e1 e2 = MulAlt e1 e2

instance HasVars VarExprT where
    var s = VarAlt s

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit e     = \_ -> Just e
    add e1 e2 = \m -> case (e1 m, e2 m) of
                          ((Just x), (Just y)) -> Just (x + y)
                          _                    -> Nothing
    mul e1 e2 = \m -> case (e1 m, e2 m) of
                          ((Just x), (Just y)) -> Just (x * y)
                          _                    -> Nothing

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
