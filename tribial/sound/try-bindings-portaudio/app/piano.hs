{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Map qualified as Map

main :: IO ()
main = putStrLn "piano"

type Sound = Map.Map Doremi PhaseLoudness

data Doremi = Do | Re | Mi | Fa | So | La | Ti | HDo deriving Show

data PhaseLoudness =
	PhaseLoudness { plPhase :: Int, plLoudness :: Loudness }
	deriving Show

data Loudness
	= Constant Float
	| Changing {
		changingNow :: Float,
		changingDiff :: Float,
		changingTo :: Float }
	deriving Show
