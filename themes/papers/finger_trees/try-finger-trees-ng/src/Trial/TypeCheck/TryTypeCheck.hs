{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Trial.TypeCheck.TryTypeCheck where

import Data.Maybe
import Data.List

import Trial.TypeCheck.ExpParser
import Data.Derivation.Wanted
import Data.Derivation.Given
import Data.Derivation.Derive

wanted :: Wanted String
Just (Just wanted, _) = expToWanted . fst <$> parseBool (tokens "((p + d) == u)")

wanted2 :: Wanted String
Just (Just wanted2, _) = expToWanted . fst <$> parseBool (tokens "((n + nn) == (b + f))")

given2 :: Given String
given2 = expsToGiven . (fst <$>) . catMaybes $ (parseBool . tokens) <$> [
	"((1 + n) == b)", "((1 + m) == d)", "(nn == (1 + f))", "(mm == (1 + h))"
	]

wanted3 :: Wanted String
Just (Just wanted3, _) = expToWanted . fst <$> parseBool (tokens "((u + lm) == z)")

given3 :: Given String
given3 = expsToGiven . (fst <$>) . catMaybes $ (parseBool . tokens) <$> [
	"(k == (m - 1))", "(u == (m + 1))", "(lm == (mm - 1))", "(li == (n - 1))",
	"(u == (m + 1))", "(z == (m + mm))", "(1 <= mm)", "(1 <= u)", "(1 <= z)"
--	"(0 <= mm)", "(0 <= u)", "(0 <= z)"
	]

debugIt :: Bool
debugIt = canDerive debugGiven debugWanted

debugRemoveVar :: [Maybe String]
debugRemoveVar = Data.Derivation.Given.containVars debugGiven \\ Data.Derivation.Wanted.containVars debugWanted

debugCheck :: Given String
debugCheck = removeVars debugGiven [
	Nothing,
	Just "m"
--	Just "moops'"
--	Just "n"
--	Just "zfsk_aOHZ",
--	Just "zfsk_aOI1"
	]
