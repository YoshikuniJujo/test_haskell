{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Trial.TypeCheck.TryTypeCheck where

import Data.Maybe
import Data.List

import Data.Derivation.CanDerive
import Data.Derivation.Parse

wanted :: Wanted String
Just (WantedSet (Just wanted)  _) = expToWanted <$> parse bool "((p + d) == u)"

wanted2 :: Wanted String
Just (WantedSet (Just wanted2) _) = expToWanted <$> parse bool "((n + nn) == (b + f))"

given2 :: Given String
given2 = expsToGiven . catMaybes $ parse bool <$> [
	"((1 + n) == b)", "((1 + m) == d)", "(nn == (1 + f))", "(mm == (1 + h))"
	]

wanted3 :: Wanted String
Just (WantedSet (Just wanted3) _) = expToWanted <$> parse bool "((u + lm) == z)"

given3 :: Given String
given3 = expsToGiven . catMaybes $ parse bool <$> [
	"(k == (m - 1))", "(u == (m + 1))", "(lm == (mm - 1))", "(li == (n - 1))",
	"(u == (m + 1))", "(z == (m + mm))", "(1 <= mm)", "(1 <= u)", "(1 <= z)"
--	"(0 <= mm)", "(0 <= u)", "(0 <= z)"
	]

debugIt :: Bool
debugIt = canDeriveGen debugGiven debugWanted

debugRemoveVar :: [Maybe String]
debugRemoveVar = Data.Derivation.CanDerive.containVarsG debugGiven \\ Data.Derivation.CanDerive.containVarsW debugWanted

debugCheck :: Given String
debugCheck = removeVars debugGiven [
	Nothing,
	Just "m"
--	Just "moops'"
--	Just "n"
--	Just "zfsk_aOHZ",
--	Just "zfsk_aOI1"
	]
