{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Derive where

import Data.List

import New.Polynominal.Given as G
import New.Polynominal.Wanted as W

canDerive :: Ord v => Given v -> Wanted v -> Bool
canDerive g w =
	selfContained w ||
	wantedToZero w `elem` givenToZeros (removeVars g rv)
	where rv = G.containVars g \\ W.containVars w
