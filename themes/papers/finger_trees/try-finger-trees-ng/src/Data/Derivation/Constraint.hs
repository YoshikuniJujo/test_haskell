{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Constraint (
	Constraint, equal, greatEqualThan, greatThan, getVars, hasVar,
	removeVar, removeNegative, isDerivableFrom, selfContained,
	Polynomial, (.+), (.-) ) where

import Prelude hiding (null, filter)

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Map.Strict (Map, null, singleton, (!?), filter, lookupMin)
import Data.Map.Merge.Strict (
	merge,
	mapMissing, preserveMissing, zipWithMatched, zipWithMaybeMatched )

import qualified Data.Map.Strict as M (toList)

---------------------------------------------------------------------------
--
-- * CONSTRAINT
--	+ DATA TYPE
--	+ CONSTRUCTOR
--	+ VARIABLE
--	+ REMOVE VAR
--	+ REMOVE NEGATIVE, IS DERIVABLE FROM AND SELF CONTAINED
-- * POLYNOMIAL
--
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

-- DATA TYPE

data Constraint v = Eq (Polynomial v) | Geq (Polynomial v)
	deriving (Show, Eq, Ord)

-- CONSTRUCTOR

equal :: Ord v => Polynomial v -> Polynomial v -> Constraint v
l `equal` r = Eq . formatEq $ l .- r

greatEqualThan :: Ord v => Polynomial v -> Polynomial v -> Constraint v
l `greatEqualThan` r = Geq . formatGeq $ l .- r

greatThan :: Ord v => Polynomial v -> Polynomial v -> Constraint v
l `greatThan` r = Geq . formatGeq $ l .- r .- singleton Nothing 1

formatEq :: Polynomial v -> Polynomial v
formatEq p =
	maybe p ((p `divide` divisor p `times`) . signum . snd) $ lookupMin p

formatGeq :: Polynomial v -> Polynomial v
formatGeq p = p `divide` divisor p

times, divide :: Polynomial v -> Integer -> Polynomial v
p `times` n = (* n) <$> p
p `divide` n = (`div` n) <$> p

divisor :: Polynomial v -> Integer
divisor = gcdAll . toList where gcdAll = \case [] -> 1; n : ns -> foldr gcd n ns

-- VARIABLE

getVars :: Ord v => Constraint v -> [Maybe v]
getVars (Eq p) = (fst <$>) $ M.toList p
getVars (Geq p) = (fst <$>) $ M.toList p

hasVar :: Ord v => Constraint v -> Maybe v -> Bool
hasVar (Eq p) v = isJust $ p !? v
hasVar (Geq p) v = isJust $ p !? v

-- REMOVE VAR

removeVar ::
	Ord v => Constraint v -> Constraint v -> Maybe v -> Maybe (Constraint v)
removeVar (Eq l) (Eq r) v = Eq . formatEq . uncurry (.+) <$> alignEE l r v
removeVar (Eq l) (Geq r) v = Geq . formatGeq . uncurry (.+) <$> alignEG l r v
removeVar (Geq l) (Geq r) v = Geq . formatGeq . uncurry (.+) <$> alignGG l r v
removeVar l r v = removeVar r l v

type Aligned v = Maybe (Polynomial v, Polynomial v)

alignEE :: Ord v => Polynomial v -> Polynomial v -> Maybe v -> Aligned v
alignEE l r v = (<$> ((,) <$> l !? v <*> r !? v)) \(nl, nr) ->
	(l `times` nr, r `times` (- nl))

alignEG :: Ord v => Polynomial v -> Polynomial v -> Maybe v -> Aligned v
alignEG l r v = (<$> ((,) <$> l !? v <*> r !? v)) \(nl, nr) ->
	(l `times` (- signum nl * nr), r `times` abs nl)

alignGG :: Ord v => Polynomial v -> Polynomial v -> Maybe v -> Aligned v
alignGG l r v = (,) <$> l !? v <*> r !? v >>= \(nl, nr) -> do
	guard $ nl * nr < 0
	pure (l `times` abs nr, r `times` abs nl)

-- REMOVE NEGATIVE, IS DERIVABLE FROM AND SELF CONTAINED

removeNegative :: Constraint v -> Constraint v
removeNegative = \case eq@(Eq _) -> eq; Geq p -> Geq $ filter (>= 0) p

isDerivableFrom :: Ord v => Constraint v -> Constraint v -> Bool
Eq w `isDerivableFrom` Eq g = w == g
Geq w `isDerivableFrom` Geq g = w `isGeqThan` g
_ `isDerivableFrom` _ = False

isGeqThan :: Ord v => Polynomial v -> Polynomial v -> Bool
l `isGeqThan` r = and $ merge
	(mapMissing \_ nl -> (nl >= 0))
	(mapMissing \_ nr -> (nr <= 0)) (zipWithMatched \_ -> (>=)) l r

selfContained :: Constraint v -> Bool
selfContained = \case Eq p -> null p; Geq p -> and $ (>= 0) <$> p

---------------------------------------------------------------------------
-- POLYNOMIAL
---------------------------------------------------------------------------

type Polynomial v = Map (Maybe v) Integer

(.+), (.-) :: Ord v => Polynomial v -> Polynomial v -> Polynomial v
(.+) = merge preserveMissing preserveMissing (zipWithMaybeMatched \_ a b -> removeZero $ a + b)
(.-) = merge preserveMissing (mapMissing \_ b -> negate b) (zipWithMaybeMatched \_ a b -> removeZero $ a - b)

removeZero :: (Eq n, Num n) => n -> Maybe n
removeZero 0 = Nothing
removeZero n = Just n
