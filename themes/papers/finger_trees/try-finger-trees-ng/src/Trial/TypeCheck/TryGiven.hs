{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TypeCheck.TryGiven where

import Data.Derivation.CanDerive
import Data.Derivation.Parse

createGiven :: String -> Maybe (Given String)
createGiven = parseIt given

sampleGiven :: Given String
Just sampleGiven =
	createGiven . ("given: {" ++) . (++ "}") $ unwords sampleExps1

sampleExps1 :: [String]
sampleExps1 = [
	"m + n == 1",
	"m + m + n == 2",
	"m + n + n <= 5" ]

{-
sampleGiven1 :: Given String
Just sampleGiven1 = createGiven sampleExps1

sampleGiven2 :: Given String
Just sampleGiven2 = createGiven [
	"(((m + n) <= 8) == F)",
	"(((m + n) + n) <= 15)"
	]
-}
