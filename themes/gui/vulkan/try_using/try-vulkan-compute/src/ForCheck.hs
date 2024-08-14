{-# LANGUAGE BangPatterns, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ForCheck (getRandomRs, checkSorted) where

import System.Random

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

checkSorted :: Ord a => Int -> [a] -> (Int, Bool, [a])
checkSorted i [] = (i, True, [])
checkSorted i [_] = (i, True, [])
checkSorted i (x : xs@(y : _))
	| x <= y = checkSorted (i + 1) xs
	| otherwise = (i, False, [x, y])
