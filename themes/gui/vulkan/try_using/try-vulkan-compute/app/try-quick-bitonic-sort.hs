{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Foldable (for_)
import Data.Word (Word32)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Random (Random, randomRs, getStdGen)
import System.Environment (getArgs)

import TryQuicksort (quicksort)
import TryBitonicsortCpu (bitonicsortCpu)
import TryBitonicsortGpu (bitonicsortGpu)

listSize :: Integral n => n
listSize = 25

quickToInsert :: Int
quickToInsert = 100

main :: IO ()
main = do
	pln : _ <- getArgs
	rs <- evaluate . force
		=<< getRandomRs (1, 10 ^ (8 :: Int)) (2 ^ (listSize :: Int))
	for_ pln \case
		'q' -> time $ quicksort quickToInsert rs
		'c' -> time $ bitonicsortCpu listSize rs
		'g' -> time $ bitonicsortGpu listSize rs
		_ -> pure ()

getRandomRs :: Random a => (a, a) -> Int -> IO [a]
getRandomRs r n = take n . randomRs r <$> getStdGen

time :: IO [Word32] -> IO ()
time a = do
	(b, ns, e) <- (,,) <$> getCurrentTime <*> a <*> getCurrentTime
	print $ take 20 ns
	print $ checkSorted 0 ns
	print $ e `diffUTCTime` b

checkSorted :: Ord a => Int -> [a] -> (Int, Bool)
checkSorted i [] = (i - 1, True)
checkSorted i [_] = (i, True)
checkSorted i (x : xs@(y : _))
	| x <= y = checkSorted (i + 1) xs | otherwise = (i, False)
