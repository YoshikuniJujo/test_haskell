{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Zero (Zero, equal, greatEqualThan, greatThan) where

import Data.Foldable
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
