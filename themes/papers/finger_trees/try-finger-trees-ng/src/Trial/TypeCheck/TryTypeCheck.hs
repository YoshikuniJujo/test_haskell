{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module Trial.TypeCheck.TryTypeCheck where

import Data.Maybe

import Data.Derivation.CanDerive
import Data.Derivation.Parse

{-
wanted :: Wanted1 String
Just (Wanted (wanted :  _)) = expToWanted =<< parse bool "((p + d) == u)"

wanted2 :: Wanted1 String
Just (Wanted (wanted2 : _)) = expToWanted =<< parse bool "((n + nn) == (b + f))"

given2 :: Given String
given2 = expsToGiven . catMaybes $ parse bool <$> [
	"((1 + n) == b)", "((1 + m) == d)", "(nn == (1 + f))", "(mm == (1 + h))"
	]

wanted3 :: Wanted1 String
Just (Wanted (wanted3 : _)) = expToWanted =<< parse bool "((u + lm) == z)"
-}

given3 :: Given String
given3 = mkGiven . catMaybes $ parseIt bool <$> [
	"(k == (m - 1))", "(u == (m + 1))", "(lm == (mm - 1))", "(li == (n - 1))",
	"(u == (m + 1))", "(z == (m + mm))", "(1 <= mm)", "(1 <= u)", "(1 <= z)" ]
