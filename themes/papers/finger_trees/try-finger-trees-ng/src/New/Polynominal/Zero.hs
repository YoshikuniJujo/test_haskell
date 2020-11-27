{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Zero (
	Zero, equal, greatEqualThan, greatThan, containVar, removeVar ) where

import Data.Foldable
import Data.Maybe
import Data.Map.Strict hiding (foldr, toList)

import New.Polynominal.Type

data Zero v
	= Eq (Polynominal v) | Geq (Polynominal v) | Grt (Polynominal v)
	deriving (Show, Eq)

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
p1 `greatThan` p2 = Grt . regularizeG $ p1 .- p2

removeVar :: Ord v => Zero v -> Zero v -> v -> Maybe (Zero v)
removeVar (Eq p1) (Eq p2) v = Eq . uncurry (.+) <$> alignVarEqEq p1 p2 v
removeVar (Eq p1) (Geq p2) v = Geq . uncurry (.+) <$> alignVarEqG p1 p2 v
removeVar (Eq p1) (Grt p2) v = Grt . uncurry (.+) <$> alignVarEqG p1 p2 v
removeVar (Geq p1) (Geq p2) v = Geq . uncurry (.+) <$> alignVarGG p1 p2 v
removeVar (Geq p1) (Grt p2) v = Grt . uncurry (.+) <$> alignVarGG p1 p2 v
removeVar (Grt p1) (Grt p2) v = Grt . uncurry (.+) <$> alignVarGG p1 p2 v
removeVar z1 z2 v = removeVar z2 z1 v

containVar :: Ord v => Zero v -> v -> Bool
containVar (Eq p) v = isJust $ p !? Just v
containVar (Geq p) v = isJust $ p !? Just v
containVar (Grt p) v = isJust $ p !? Just v

alignVarEqEq :: Ord v => Polynominal v -> Polynominal v -> v -> Maybe (Polynominal v, Polynominal v)
alignVarEqEq p1 p2 v = case (p1 !? Just v, p2 !? Just v) of
	(Just n1, Just n2) -> Just (p1 `multiple` n2, p2 `multiple` (- n1))
	_ -> Nothing

alignVarEqG :: Ord v => Polynominal v -> Polynominal v -> v -> Maybe (Polynominal v, Polynominal v)
alignVarEqG p1 p2 v = case (p1 !? Just v, p2 !? Just v) of
	(Just n1, Just n2) -> Just (
		p1 `multiple` (- signum n1 * n2), p2 `multiple` abs n1 )
	_ -> Nothing

alignVarGG :: Ord v => Polynominal v -> Polynominal v -> v -> Maybe (Polynominal v, Polynominal v)
alignVarGG p1 p2 v = case (p1 !? Just v, p2 !? Just v) of
	(Just n1, Just n2)
		| signum n1 * signum n2 == - 1 -> Just (
			p1 `multiple` abs n2, p2 `multiple` abs n1 )
	_ -> Nothing
