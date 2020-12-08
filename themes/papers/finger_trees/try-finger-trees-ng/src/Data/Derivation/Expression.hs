{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Expression (
	Exp(..), Number, makeConstraint, makeVarBool ) where

import Outputable (Outputable(..), text)
import Control.Arrow (first, second)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Maybe (fromJust)
import Data.List (find)
import Data.Map.Strict (Map, (!?), empty, singleton, insert)

import Data.Derivation.Constraint (
	Constraint, equal, greatEqualThan, greatThan, Polynomial, (.+), (.-) )

---------------------------------------------------------------------------

-- * DATA EXP
-- * MAKE CONSTRAInT
-- * MAKE POLYNOMIAL
-- * MAKE VAR BOOL

---------------------------------------------------------------------------
-- DATA EXP
---------------------------------------------------------------------------

data Exp v t where
	Bool :: Bool -> Exp v Bool
	Var :: v -> Exp v a
	Const :: Integer -> Exp v Number
	(:==) :: Exp v a -> Exp v a -> Exp v Bool
	(:<=) :: Exp v Number -> Exp v Number -> Exp v Bool
	(:+) :: Exp v Number -> Exp v Number -> Exp v Number
	(:-) :: Exp v Number -> Exp v Number -> Exp v Number

data Number

deriving instance Show v => Show (Exp v t)
instance Show v => Outputable (Exp v t) where ppr = text . show

---------------------------------------------------------------------------
-- MAKE CONSTRAINT
---------------------------------------------------------------------------

makeConstraint :: Ord v =>
	VarBool v -> Exp v Bool -> (Maybe (Constraint v), [Constraint v])
makeConstraint vb e = runWriter $ procProp vb e True

procProp :: Ord v => VarBool v ->
	Exp v Bool -> Bool -> Writer [Constraint v] (Maybe (Constraint v))
procProp _ (Bool _) _ = pure Nothing; procProp _ (Var _) _ = pure Nothing
procProp _ (l :<= r) False = Just <$> (greatThan <$> poly l <*> poly r)
procProp _ (l :<= r) True = Just <$> (greatEqualThan <$> poly r <*> poly l)
procProp vb (l :== Bool r) b = procProp vb l (r == b)
procProp vb (Bool l :== r) b = procProp vb r (l == b)
procProp vb (l :== Var r) b | Just br <- vb !? r = case l of
	_ :== _ -> procProp vb l (br == b); _ :<= _ -> procProp vb l (br == b)
	_ -> pure Nothing
procProp vb (Var l :== r) b | Just bl <- vb !? l = case r of
	_ :== _ -> procProp vb r (bl == b); _ :<= _ -> procProp vb r (bl == b)
	_ -> pure Nothing
procProp _ (l :== r) True = case (l, r) of
	 (Const _, _) -> Just <$> (equal <$> poly l <*> poly r)
	 (_ :+ _, _) -> Just <$> (equal <$> poly l <*> poly r)
	 (_ :- _, _) -> Just <$> (equal <$> poly l <*> poly r)
	 (_, Const _) -> Just <$> (equal <$> poly l <*> poly r)
	 (_, _ :+ _) -> Just <$> (equal <$> poly l <*> poly r)
	 (_, _ :- _) -> Just <$> (equal <$> poly l <*> poly r)
	 (Var v, Var w) -> Just <$> (equal <$> poly (Var v) <*> poly (Var w))
	 _ -> pure Nothing
procProp _ (_ :== _) False = pure Nothing

---------------------------------------------------------------------------
-- MAKE POLYNOMIAL
---------------------------------------------------------------------------

poly :: Ord v => Exp v Number -> Writer [Constraint v] (Polynomial v)
poly (Const n) = pure $ singleton Nothing n
poly (Var v) = do
	let	v' = singleton (Just v) 1
	tell [v' `greatEqualThan` empty]
	pure v'
poly (t1 :+ t2) = (.+)  <$> poly t1 <*> poly t2
poly (t1 :- t2) = do
	t1' <- poly t1
	t2' <- poly t2
	tell [t1' `greatEqualThan` t2']
	pure $ t1' .- t2'

---------------------------------------------------------------------------
-- MAKE VAR BOOL
---------------------------------------------------------------------------

type VarBool v = Map v Bool

makeVarBool, expToVarBool :: Ord v => [Exp v Bool] -> VarBool v
makeVarBool = expToVarBool
expToVarBool = snd . untilFixed (uncurry expToVarBoolStep) . expToVarBoolInit

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
