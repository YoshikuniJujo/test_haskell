{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal where

import Data.Map.Strict
import Data.Map.Merge.Strict

import New.Expression

type Polynominal v = Map (Maybe v) Integer

termToPolynominal :: Ord v => Exp v Term -> Polynominal v
termToPolynominal (Const n) = singleton Nothing n
termToPolynominal (Var v) = singleton (Just v) 1
termToPolynominal (t1 :+ t2) = termToPolynominal t1 .+ termToPolynominal t2
termToPolynominal (t1 :- t2) = termToPolynominal t1 .- termToPolynominal t2

(.+), (.-) :: Ord v => Polynominal v -> Polynominal v -> Polynominal v
(.+) = merge preserveMissing preserveMissing (zipWithMaybeMatched \_ a b -> removeZero $ a + b)
(.-) = merge preserveMissing (mapMissing \_ b -> negate b) (zipWithMaybeMatched \_ a b -> removeZero $ a - b)

removeZero :: (Eq n, Num n) => n -> Maybe n
removeZero 0 = Nothing
removeZero n = Just n
