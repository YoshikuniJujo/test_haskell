{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Constraint (
	Constraint, equal, greatEqualThan, greatThan, getVars, hasVar,
	removeVar, removeNegative, isDerivableFrom, selfContained,
	Polynomial, (.+), (.-) ) where

import Prelude hiding (null, filter)

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
p1 `equal` p2 = Eq . regularizeEq $ p1 .- p2

greatEqualThan :: Ord v => Polynomial v -> Polynomial v -> Constraint v
p1 `greatEqualThan` p2 = Geq . regularizeG $ p1 .- p2

greatThan :: Ord v => Polynomial v -> Polynomial v -> Constraint v
p1 `greatThan` p2 = Geq . regularizeG $ p1 .- p2 .- one

gcdAll :: Integral n => [n] -> Maybe n
gcdAll [] = Nothing
gcdAll (n : ns) = Just $ foldr gcd n ns

divisor :: Polynomial v -> Maybe Integer
divisor = gcdAll . toList

divide :: Polynomial v -> Integer -> Polynomial v
p `divide` n = (`div` n) <$> p

multiple :: Polynomial v -> Integer -> Polynomial v
p `multiple` n = (* n) <$> p

regularizeEq :: Polynomial v -> Polynomial v
regularizeEq p = case (lookupMin p, divisor p) of
	(Just (_, mn), Just d) -> p `multiple` signum mn `divide` d
	_ -> p

regularizeG :: Polynomial v -> Polynomial v
regularizeG p = case divisor p of
	Just d -> p `divide` d
	Nothing -> p

-- VARIABLE

getVars :: Ord v => Constraint v -> [Maybe v]
getVars (Eq p) = (fst <$>) $ M.toList p
getVars (Geq p) = (fst <$>) $ M.toList p

hasVar :: Ord v => Constraint v -> Maybe v -> Bool
hasVar (Eq p) v = isJust $ p !? v
hasVar (Geq p) v = isJust $ p !? v

-- REMOVE VAR

removeVar :: Ord v => Constraint v -> Constraint v -> Maybe v -> Maybe (Constraint v)
removeVar (Eq p1) (Eq p2) v = Eq . regularizeEq . uncurry (.+) <$> alignVarEqEq p1 p2 v
removeVar (Eq p1) (Geq p2) v = Geq . regularizeG . uncurry (.+) <$> alignVarEqG p1 p2 v
removeVar (Geq p1) (Geq p2) v = Geq . regularizeG . uncurry (.+) <$> alignVarGG p1 p2 v
removeVar z1 z2 v = removeVar z2 z1 v

alignVarEqEq :: Ord v => Polynomial v -> Polynomial v -> Maybe v -> Maybe (Polynomial v, Polynomial v)
alignVarEqEq p1 p2 v = case (p1 !? v, p2 !? v) of
	(Just n1, Just n2) -> Just (p1 `multiple` n2, p2 `multiple` (- n1))
	_ -> Nothing

alignVarEqG :: Ord v => Polynomial v -> Polynomial v -> Maybe v -> Maybe (Polynomial v, Polynomial v)
alignVarEqG p1 p2 v = case (p1 !? v, p2 !? v) of
	(Just n1, Just n2) -> Just (
		p1 `multiple` (- signum n1 * n2), p2 `multiple` abs n1 )
	_ -> Nothing

alignVarGG :: Ord v => Polynomial v -> Polynomial v -> Maybe v -> Maybe (Polynomial v, Polynomial v)
alignVarGG p1 p2 v = case (p1 !? v, p2 !? v) of
	(Just n1, Just n2)
		| signum n1 * signum n2 == - 1 -> Just (
			p1 `multiple` abs n2, p2 `multiple` abs n1 )
	_ -> Nothing

-- REMOVE NEGATIVE, IS DERIVABLE FROM AND SELF CONTAINED

removeNegative :: Constraint v -> Constraint v
removeNegative eq@(Eq _) = eq
removeNegative (Geq p) = Geq $ filter (> 0) p

isDerivableFrom :: Ord v => Constraint v -> Constraint v -> Bool
Eq pw `isDerivableFrom` Eq pg = pw == pg
Geq pw `isDerivableFrom` Geq pg = pw `isEqLargerThan` pg
isDerivableFrom _ _ = False

isEqLargerThan :: Ord v => Polynomial v -> Polynomial v -> Bool
p1 `isEqLargerThan` p2 = foldr (&&) True $ merge (mapMissing \_ n -> True) (mapMissing \_ n -> True) (zipWithMatched \_ -> (>=)) p1 p2

selfContained :: Constraint v -> Bool
selfContained (Eq p) = null p
selfContained (Geq p) = checkAll (>= 0) p

checkAll :: (v -> Bool) -> Map k v -> Bool
checkAll p = and . (p <$>)

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

one :: Polynomial v
one = singleton Nothing 1
