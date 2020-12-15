{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Constraint where

import Prelude hiding (null, filter, (<>))

import Outputable (Outputable(..), (<>), (<+>), text)
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Map.Strict (Map, null, singleton, (!?), filter, lookupMin)
import Data.Map.Merge.Strict

import qualified Data.Map.Strict as M

type Polynomial v = Map (Maybe v) Integer

(.+), (.-) :: Ord v => Polynomial v -> Polynomial v -> Polynomial v
(.+) = merge preserveMissing preserveMissing
	(zipWithMaybeMatched \_ a b -> rmZero $ a + b)
(.-) = merge preserveMissing (mapMissing $ const negate)
	(zipWithMaybeMatched \_ a b -> rmZero $ a - b)

rmZero :: (Eq n, Num n) => n -> Maybe n
rmZero = \case 0 -> Nothing; n -> Just n

data Constraint v = Eq (Polynomial v) | Geq (Polynomial v)
	deriving (Show, Eq, Ord)

equal :: Ord v => Polynomial v -> Polynomial v -> Constraint v
l `equal` r = Eq . formatEq $ l .- r

greatEqualThan :: Ord v => Polynomial v -> Polynomial v -> Constraint v
l `greatEqualThan` r = Geq . formatGeq $ l .- r

greatThan :: Ord v => Polynomial v -> Polynomial v -> Constraint v
l `greatThan` r = Geq $ formatGeq (l .- r) .- singleton Nothing 1

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
