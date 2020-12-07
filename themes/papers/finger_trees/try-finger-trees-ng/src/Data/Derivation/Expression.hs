{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Expression (
	Exp(..), eqToZero', expToVarBool, Term, termToPolynomial ) where

import Outputable hiding (empty)
import Control.Arrow
import Control.Monad.Writer
import Data.Maybe
import Data.List hiding (insert)
import Data.Map.Strict

import Data.Derivation.Constraint

data Term

data Exp v t where
	Bool :: Bool -> Exp v Bool
	Const :: Integer -> Exp v Term
	Var :: v -> Exp v a
	(:+) :: Exp v Term -> Exp v Term -> Exp v Term
	(:-) :: Exp v Term -> Exp v Term -> Exp v Term
	(:<=) :: Exp v Term -> Exp v Term -> Exp v Bool
	(:==) :: Exp v a -> Exp v a -> Exp v Bool

deriving instance Show v => Show (Exp v t)

instance Show v => Outputable (Exp v t) where
	ppr = text . show

termToPolynomial :: Ord v => Exp v Term -> Writer [Constraint v] (Polynomial v)
termToPolynomial (Const n) = pure $ singleton Nothing n
termToPolynomial (Var v) = do
	let	v' = singleton (Just v) 1
	tell [v' `greatEqualThan` empty]
	pure v'
termToPolynomial (t1 :+ t2) = (.+)  <$> termToPolynomial t1 <*> termToPolynomial t2
termToPolynomial (t1 :- t2) = do
	t1' <- termToPolynomial t1
	t2' <- termToPolynomial t2
	tell [t1' `greatEqualThan` t2']
	pure $ t1' .- t2'

eqToZero' :: Ord v => Exp v Bool -> Bool -> VarBool v -> (Maybe (Constraint v), [Constraint v])
-- eqToZero' e b vb = uncurry (maybe id (:)) $ runWriter (eqToZero e b vb)
eqToZero' e b vb = runWriter (eqToZero e b vb)

eqToZero :: Ord v => Exp v Bool -> Bool -> VarBool v -> Writer [Constraint v] (Maybe (Constraint v))
eqToZero (Bool _) _ _ = pure Nothing
eqToZero (Var _) _ _ = pure Nothing
eqToZero (t1 :<= t2) False _ = Just <$> (greatThan <$> termToPolynomial t1 <*> termToPolynomial t2)
eqToZero (t1 :<= t2) True _ = Just <$> (greatEqualThan <$> termToPolynomial t2 <*> termToPolynomial t1)
eqToZero (b1 :== Bool b2) b vb = eqToZero b1 (b2 == b) vb
eqToZero (Bool b1 :== b2) b vb = eqToZero b2 (b1 == b) vb
eqToZero (b1 :== Var v2) b vb | Just b2 <- vb !? v2 = case b1 of
	_ :<= _ -> eqToZero b1 (b2 == b) vb
	_ :== _ -> eqToZero b1 (b2 == b) vb
	_ -> pure Nothing
eqToZero (Var v1 :== b2) b vb | Just b1 <- vb !? v1 = case b2 of
	_ :<= _ -> eqToZero b2 (b1 == b) vb
	_ :== _ -> eqToZero b2 (b1 == b) vb
	_ -> pure Nothing
eqToZero (t1 :== t2) True _ = case (t1, t2) of
	 (Const _, _) -> Just <$> (equal <$> termToPolynomial t1 <*> termToPolynomial t2)
	 (_ :+ _, _) -> Just <$> (equal <$> termToPolynomial t1 <*> termToPolynomial t2)
	 (_ :- _, _) -> Just <$> (equal <$> termToPolynomial t1 <*> termToPolynomial t2)
	 (_, Const _) -> Just <$> (equal <$> termToPolynomial t1 <*> termToPolynomial t2)
	 (_, _ :+ _) -> Just <$> (equal <$> termToPolynomial t1 <*> termToPolynomial t2)
	 (_, _ :- _) -> Just <$> (equal <$> termToPolynomial t1 <*> termToPolynomial t2)
	 (Var v1, Var v2) -> Just <$> (equal <$> termToPolynomial (Var v1) <*> termToPolynomial (Var v2))
	 _ -> pure Nothing
eqToZero _ False _ = pure Nothing

type VarBool v = Map v Bool

expToVarBoolInit :: Ord v => [Exp v Bool] -> ([(v, v)], VarBool v)
expToVarBoolInit [] = ([], empty)
expToVarBoolInit (Var v1 :== Var v2 : es) = ((v1, v2) :) `first` expToVarBoolInit es
expToVarBoolInit (Var v1 :== Bool b2 : es) = insert v1 b2 `second` expToVarBoolInit es
expToVarBoolInit (Bool b1 :== Var v2 : es) = insert v2 b1 `second` expToVarBoolInit es
expToVarBoolInit (_ : es) = expToVarBoolInit es

expToVarBoolStep :: Ord v => [(v, v)] -> VarBool v -> ([(v, v)], VarBool v)
expToVarBoolStep [] vb = ([], vb)
expToVarBoolStep ((v1, v2) : vs) vb = case vb !? v1 of
	Just b -> expToVarBoolStep vs (insert v2 b vb)
	Nothing -> case vb !? v2 of
		Just b -> expToVarBoolStep vs (insert v1 b vb)
		Nothing -> ((v1, v2) :) `first` expToVarBoolStep vs vb

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f x = fst . fromJust . find (uncurry (==)) $ zip xs (tail xs)
	where xs = iterate f x

expToVarBool :: Ord v => [Exp v Bool] -> VarBool v
expToVarBool = snd . untilFixed (uncurry expToVarBoolStep) . expToVarBoolInit
