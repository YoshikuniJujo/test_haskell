{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal where

import Control.Arrow
import Data.Maybe
import Data.List hiding (insert)
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

data Zero v
	= Eq (Polynominal v) | Geq (Polynominal v) | Grt (Polynominal v)
	deriving Show

type VarBool v = Map v Bool

termToVarBoolInit :: Ord v => [Exp v Bool] -> ([(v, v)], VarBool v)
termToVarBoolInit [] = ([], empty)
termToVarBoolInit (Var v1 :== Var v2 : es) = ((v1, v2) :) `first` termToVarBoolInit es
termToVarBoolInit (Var v1 :== Bool b2 : es) = insert v1 b2 `second` termToVarBoolInit es
termToVarBoolInit (Bool b1 :== Var v2 : es) = insert v2 b1 `second` termToVarBoolInit es
termToVarBoolInit (_ : es) = termToVarBoolInit es

termToVarBoolStep :: Ord v => [(v, v)] -> VarBool v -> ([(v, v)], VarBool v)
termToVarBoolStep [] vb = ([], vb)
termToVarBoolStep ((v1, v2) : vs) vb = case vb !? v1 of
	Just b -> termToVarBoolStep vs (insert v2 b vb)
	Nothing -> case vb !? v2 of
		Just b -> termToVarBoolStep vs (insert v1 b vb)
		Nothing -> ((v1, v2) :) `first` termToVarBoolStep vs vb

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f x = fst . fromJust . find (uncurry (==)) $ zip xs (tail xs)
	where xs = iterate f x

sampleExpVarBool :: [Exp String Bool]
sampleExpVarBool = [
	Var "bar" :== Var "aaaaa",
	Var "foo" :== Const 8,
	Var "bar" :== Var "hoge",
	Var "hoge" :== Bool False,
	Bool True :== Var "piyo"
	]

termToVarBool :: Ord v => [Exp v Bool] -> VarBool v
termToVarBool = snd . untilFixed (uncurry termToVarBoolStep) . termToVarBoolInit
