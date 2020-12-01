{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Polynominal.Derive where

import Data.List

import Polynominal.Zero
import Polynominal.Given as G
import Polynominal.Wanted as W

canDerive :: Ord v => Given v -> Wanted v -> Bool
canDerive g w =
	selfContained w ||
	elemBy isDerivableFrom (wantedToZero w) (givenToZeros $ removeVars g rv)
--	wantedToZero w `elem` givenToZeros (removeVars g rv)
	where rv = G.containVars g \\ W.containVars w

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq = any . eq
