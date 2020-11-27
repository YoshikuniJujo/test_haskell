{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Wanted where

import Data.Map.Strict

import New.Polynominal
import New.Polynominal.Zero
import New.Expression

newtype Wanted v = Wanted (Zero v) deriving Show

expToWanted :: Ord v => Exp v Bool -> Maybe (Wanted v)
expToWanted = (Wanted <$>) . \e -> eqToZero e True empty

wantedToZero :: Wanted v -> Zero v
wantedToZero (Wanted z) = z
