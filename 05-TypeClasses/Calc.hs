{-# LANGUAGE TypeSynonymInstances #-}
module Calc where

import Distribution.PackageDescription.Configuration (addBuildableCondition)
import ExprT
import Language.Haskell.TH (Exp, litE)
import Parser
import StackVM

-- EX 1

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- EX 2

evalStr :: String -> Maybe Integer
evalStr s = parsedToMaybeInteger (parseExp ExprT.Lit ExprT.Add ExprT.Mul s)
  where
    parsedToMaybeInteger (Just e) = Just (eval e)
    parsedToMaybeInteger Nothing = Nothing

-- EX 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- EX 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul :: MinMax -> MinMax -> MinMax
  mul = min

instance Expr Mod7 where
  lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- EX 5

instance Expr Program where 
  lit a = [StackVM.PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul