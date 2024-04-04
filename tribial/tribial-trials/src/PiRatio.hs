{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PiRatio where

piapps :: Double -> Double -> Int -> [(Int, Int)]
piapps mnd d n
	| d' < d = (m, n) : piapps mnd d' (n + 1)
	| d' < mnd = []
	| otherwise = piapps mnd d (n + 1)
	where
	x = fromIntegral n * pi
	m = round x
	d' = abs $ x - fromIntegral m
