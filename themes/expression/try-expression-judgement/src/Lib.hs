{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Expression

infix 4 :=

data Equality i v = Exp i v := Exp i v deriving Show

fromEquality :: (Integral i, Ord v) => Equality i v -> Expression i v
fromEquality (e1 := e2) = toExpression e1 .- toExpression e2

infixl 6 :+, :-

data Exp i v = N i | V v | Exp i v :+ Exp i v | Exp i v :- Exp i v deriving Show

toExpression :: (Integral i, Ord v) => Exp i v -> Expression i v
toExpression (N n) = num n
toExpression (V v) = var v
toExpression (e1 :+ e2) = toExpression e1 .+ toExpression e2
toExpression (e1 :- e2) = toExpression e1 .- toExpression e2
