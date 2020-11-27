{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module New.Trial.TryTypeCheck where

import Data.Maybe

import New.Trial.ExpParser
import New.Polynominal.Wanted
import New.Polynominal.Given

wanted :: Wanted String
Just wanted = expToWanted . fst =<< parseBool (tokens "((p + d) == u)")

wanted2 :: Wanted String
Just wanted2 = expToWanted . fst =<< parseBool (tokens "((n + nn) == (b + f))")

given2 :: Given String
given2 = expsToGiven . (fst <$>) . catMaybes $ (parseBool . tokens) <$> [
	"((1 + n) == b)", "((1 + m) == d)", "(nn == (1 + f))", "(mm == (1 + h))"
	]
