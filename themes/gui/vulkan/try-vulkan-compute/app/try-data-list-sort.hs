{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.List qualified as L
import Data.Word
import System.Random

import Data.Time

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

main :: IO ()
main = do
	rs <- getRandomRs @Word32 (1, 100000) $ 2 ^ 20
	ct0 <- getCurrentTime
	print . take 20 $ L.sort rs
	print . checkSorted $ L.sort rs
	ct1 <- getCurrentTime
	print $ diffUTCTime ct1 ct0

checkSorted :: Ord a => [a] -> Bool
checkSorted [_] = True
checkSorted (x : xs@(y : _))
	| x <= y = checkSorted xs
	| otherwise = False
