{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Polynominal.Zero (
	Zero, equal, greatEqualThan, greatThan,
	containVars, doesContainVar, removeVar, identity,
	isDerivableFrom,
	noNegativeFromG, isEq, debugZeros, debugZeroWanted ) where

import Data.Foldable
import Data.Maybe
import Data.Map.Strict hiding (foldr, toList, null)
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as M

import Polynominal.Type

data Zero v
	= Eq (Polynominal v) | Geq (Polynominal v)
	deriving (Show, Eq, Ord)

isEq :: Zero v -> Bool
isEq (Eq _) = True
isEq _ = False

gcdAll :: Integral n => [n] -> Maybe n
gcdAll [] = Nothing
gcdAll (n : ns) = Just $ foldr gcd n ns

divisor :: Polynominal v -> Maybe Integer
divisor = gcdAll . toList

divide :: Polynominal v -> Integer -> Polynominal v
p `divide` n = (`div` n) <$> p

multiple :: Polynominal v -> Integer -> Polynominal v
p `multiple` n = (* n) <$> p

regularizeEq :: Polynominal v -> Polynominal v
regularizeEq p = case (lookupMin p, divisor p) of
	(Just (_, mn), Just d) -> p `multiple` signum mn `divide` d
	_ -> p

regularizeG :: Polynominal v -> Polynominal v
regularizeG p = case divisor p of
	Just d -> p `divide` d
	Nothing -> p

equal :: Ord v => Polynominal v -> Polynominal v -> Zero v
p1 `equal` p2 = Eq . regularizeEq $ p1 .- p2

greatEqualThan :: Ord v => Polynominal v -> Polynominal v -> Zero v
p1 `greatEqualThan` p2 = Geq . regularizeG $ p1 .- p2

greatThan :: Ord v => Polynominal v -> Polynominal v -> Zero v
p1 `greatThan` p2 = Geq . regularizeG $ p1 .- p2 .- one

removeVar :: Ord v => Zero v -> Zero v -> Maybe v -> Maybe (Zero v)
removeVar (Eq p1) (Eq p2) v = Eq . regularizeEq . uncurry (.+) <$> alignVarEqEq p1 p2 v
removeVar (Eq p1) (Geq p2) v = Geq . regularizeG . uncurry (.+) <$> alignVarEqG p1 p2 v
removeVar (Geq p1) (Geq p2) v = Geq . regularizeG . uncurry (.+) <$> alignVarGG p1 p2 v
removeVar z1 z2 v = removeVar z2 z1 v

doesContainVar :: Ord v => Zero v -> Maybe v -> Bool
doesContainVar (Eq p) v = isJust $ p !? v
doesContainVar (Geq p) v = isJust $ p !? v

containVars :: Ord v => Zero v -> [Maybe v]
containVars (Eq p) = (fst <$>) $ M.toList p
containVars (Geq p) = (fst <$>) $ M.toList p

alignVarEqEq :: Ord v => Polynominal v -> Polynominal v -> Maybe v -> Maybe (Polynominal v, Polynominal v)
alignVarEqEq p1 p2 v = case (p1 !? v, p2 !? v) of
	(Just n1, Just n2) -> Just (p1 `multiple` n2, p2 `multiple` (- n1))
	_ -> Nothing

alignVarEqG :: Ord v => Polynominal v -> Polynominal v -> Maybe v -> Maybe (Polynominal v, Polynominal v)
alignVarEqG p1 p2 v = case (p1 !? v, p2 !? v) of
	(Just n1, Just n2) -> Just (
		p1 `multiple` (- signum n1 * n2), p2 `multiple` abs n1 )
--	(_, Just n2) | n2 <= 0 -> Just (empty, p2)
	_ -> Nothing

alignVarGG :: Ord v => Polynominal v -> Polynominal v -> Maybe v -> Maybe (Polynominal v, Polynominal v)
alignVarGG p1 p2 v = case (p1 !? v, p2 !? v) of
	(Just n1, Just n2)
		| signum n1 * signum n2 == - 1 -> Just (
			p1 `multiple` abs n2, p2 `multiple` abs n1 )
--	(Just n1, _) | n1 <= 0 -> Just (p1, empty)
--	(_, Just n2) | n2 <= 0 -> Just (empty, p2)
	_ -> Nothing

identity :: Zero v -> Bool
identity (Eq p) = M.null p
identity (Geq p) = checkAll (>= 0) p -- M.null p

checkAll :: (v -> Bool) -> Map k v -> Bool
checkAll p = and . (p <$>)

zipAll :: Ord k => (v -> v -> Bool) -> Map k v -> Map k v -> Bool
zipAll p m1 m2 = foldr (&&) True $ merge (mapMissing \_ _ -> False) (mapMissing \_ _ -> False) (zipWithMatched \_ -> p) m1 m2

isEqLargerThan :: Ord v => Polynominal v -> Polynominal v -> Bool
-- p1 `isEqLargerThan` p2 = foldr (&&) True $ merge (mapMissing \_ n -> (n >= 0)) (mapMissing \_ n -> (n <= 0)) (zipWithMatched \_ -> (>=)) p1 p2
p1 `isEqLargerThan` p2 = foldr (&&) True $ merge (mapMissing \_ n -> True) (mapMissing \_ n -> True) (zipWithMatched \_ -> (>=)) p1 p2

isDerivableFrom :: Ord v => Zero v -> Zero v -> Bool
isDerivableFrom (Eq pw) (Eq pg) = pw == pg
isDerivableFrom (Geq pw) (Geq pg) = pw `isEqLargerThan` pg
isDerivableFrom _ _ = False

noNegativeFromG :: Zero v -> Zero v
noNegativeFromG eq@(Eq _) = eq
noNegativeFromG (Geq p) = Geq $ M.filter (> 0) p

debugZeros, debugZeros1, debugZeros2 :: [Zero String]
debugZeros = debugZeros1 ++ debugZeros2

debugZeros1 = Eq . fromList <$> [
	[(Nothing, 1), (Just "m", - 1), (Just "zfsk_aOI1", 1)],
	[(Nothing, 1), (Just "m", 1), (Just "zfsk_aOHL", -1)],
	[(Nothing, 1), (Just "moops'", -1), (Just "zfsk_aOI3", 1)],
	[(Nothing, 1), (Just "n", -1), (Just "zfsk_aOHZ", 1)],
	[(Just "m", 1), (Just "moops'", 1), (Just "zfsk_aOHQ", -1)]
	]

debugZeros2 = Geq . fromList <$> [
	[(Nothing, -1), (Just "moops'", 1)],
	[(Nothing, -1), (Just "zfsk_aOHL", 1)],
	[(Nothing, -1), (Just "zfsk_aOHQ", 1)],
	[(Just "moops'", 1)],
	[(Just "zfsk_aOHL", 1)],
	[(Just "zfsk_aOHQ", 1)]
	]

debugZeroWanted = Eq $ fromList [(Just "zfsk_aOHL", 1), (Just "zfsk_aOHQ", -1), (Just "zfsk_aOI3", 1)]
