{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Loudness (

	Loudness, pattern Silent,
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
changeLoudness l d t = case l of
	Constant n -> Gradation Changing {
		changingNow = n,
		changingDiff = d,
		changingTo = t }
	Gradation Changing { changingNow = n } -> Gradation Changing {
		changingNow = n,
		changingDiff = d,
		changingTo = t }

between :: Ord n => n -> n -> n -> Bool
between mn mx nw = mn <= nw && nw <= mx || mx <= nw && nw <= mn
