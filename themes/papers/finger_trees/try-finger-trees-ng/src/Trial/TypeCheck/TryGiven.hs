{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TypeCheck.TryGiven where

import Data.Map.Strict

import Trial.TypeCheck.ExpParser
import Data.Derivation.AvoidNegative
import Data.Derivation.Zero
import Data.Derivation.CanDerive

createGiven :: [String] -> Maybe (Given String)
createGiven ss = do
	zs <- createZero `mapM` ss
	pure $ given zs

createZero :: String -> Maybe (Zero String)
createZero s = do
	(e, _) <- parseBool $ tokens s
	fst $ eqToZero' e True empty

sampleExps1 :: [String]
sampleExps1 = [
	"((m + n) == 1)",
	"(((m + m) + n) == 2)",
	"(((m + n) + n) <= 5)"
	]

sampleGiven1 :: Given String
Just sampleGiven1 = createGiven sampleExps1

sampleGiven2 :: Given String
Just sampleGiven2 = createGiven [
	"(((m + n) <= 8) == F)",
	"(((m + n) + n) <= 15)"
	]
