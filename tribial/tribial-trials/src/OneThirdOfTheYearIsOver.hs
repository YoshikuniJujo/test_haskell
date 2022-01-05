{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OneThirdOfTheYearIsOver where

import Control.Arrow
import Data.Ratio
import Data.Time.Calendar hiding (Year)

type Year = Integer
type Pair = (Int, Int)

newYear, newYearsEve, lastNewYearsEve :: Year -> Day
newYear y = fromGregorian y 1 1
newYearsEve y = fromGregorian y 12 31
lastNewYearsEve = newYearsEve . subtract 1

fromDate :: Year -> Pair -> Integer
fromDate y (m, d) = fromGregorian y m d `diffDays` lastNewYearsEve y

fromRatio :: Year -> Pair -> Integer
fromRatio y (fromIntegral -> m, fromIntegral -> d) = ceiling $ days * (m % d)
	where days = newYearsEve y `diffDays` lastNewYearsEve y % 1

allPairs :: Year -> [Pair]
allPairs y = pairFromDay <$> [newYear y .. newYearsEve y]
	where pairFromDay day = let (_y, m, d) = toGregorian day in (m, d)

doubleMeanings :: Year -> [Pair]
doubleMeanings y =
	filter (uncurry (==) . (fromDate y &&& fromRatio y)) $ allPairs y

doubleMeanings' :: Year -> [Pair]
doubleMeanings' y =
	filterEdge (uncurry (>=) . (fromDate y &&& fromRatio y)) $ allPairs y

filterEdge :: (a -> Bool) -> [a] -> [a]
filterEdge p = go False
	where go b = \case
		[] -> []
		x : xs	| not b && p x -> x : go True xs
			| otherwise -> go (p x) xs

filterEdge' :: (a -> Bool) -> [a] -> [a]
filterEdge' p xs = map snd . filter fst
	$ zipWith (\b0 -> (not b0 &&)) (False : bs) bs `zip` xs
	where bs = map p xs
