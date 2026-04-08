{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Loudness (

	Loudness, pattern Silent, zero,
	currentLoudness, nextLoudness, changeLoudness

	) where

data Loudness = Constant Float | Gradation Changing deriving Show

data Changing = Changing {
	changingNow :: Float,
	changingDiff :: Float,
	changingTo :: Float }
	deriving Show

pattern Silent :: Loudness
pattern Silent = Constant 0

zero :: Loudness -> Bool
zero = \case
	Constant 0 -> True
	Gradation Changing { changingNow = 0, changingDiff = d } -> d <= 0
	_ -> False

currentLoudness :: Loudness -> Float
currentLoudness = \case
	Constant l -> l
	Gradation Changing { changingNow = l } -> l

nextLoudness :: Loudness -> Loudness
nextLoudness = \case
	l@(Constant _) -> l
	Gradation Changing {
			changingNow = n,
			changingDiff = d,
			changingTo = t }
		| between n (n + d) t -> Constant t
		| otherwise -> Gradation Changing {
			changingNow = n + d,
			changingDiff = d,
			changingTo = t }

changeLoudness :: Loudness -> Float -> Float -> Loudness
changeLoudness l d t
	| doesApproach n d t = Gradation
		Changing { changingNow = n, changingDiff = d, changingTo = t }
	| otherwise = Constant n
	where n = currentLoudness l

doesApproach :: Float -> Float -> Float -> Bool
doesApproach n d t = (t - n) * signum d > 0

between :: Ord n => n -> n -> n -> Bool
between mn mx nw = mn <= nw && nw <= mx || mx <= nw && nw <= mn
