{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, UnicodeSyntax #-}

module Calc where

import ExprT
import Parser

eval :: ExprT -> Integer           
eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

--evalStr s = case (parseExp Lit Add Mul s) of
--  Just exp -> Just (eval exp)
--  _        -> Nothing

            
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

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype Mod7 = Mod7 Integer deriving (Eq, Show, Enum, Ord, Real, Integral, Num)
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = min
  mul = max
  
instance Expr Mod7 where
  lit     = Mod7
  add i j = (i + j) `mod` 7
  mul i j = (i * j) `mod` 7

  
