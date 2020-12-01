{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Derivation.Derive where

import Data.List

import Derivation.Zero
import Derivation.Given as G
import Derivation.Wanted as W

canDerive :: Ord v => Given v -> Wanted v -> Bool
canDerive g w =
	selfContained w ||
	elemBy isDerivableFrom (wantedToZero w) (givenToZeros $ removeVars g rv)
--	wantedToZero w `elem` givenToZeros (removeVars g rv)
	where rv = G.containVars g \\ W.containVars w

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq = any . eq
