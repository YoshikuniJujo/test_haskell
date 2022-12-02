{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time
import Data.Hason
import Text.PrettyPrint

import QuickSort.Taocp
import MergeSort.Natural
import HeapSort.TaocpFree (heapsort)
import Tools

type Result = (
	NominalDiffTime, NominalDiffTime, NominalDiffTime, NominalDiffTime )

main :: IO ()
main = do
	rs <- trial `mapM` [
		10 ^ i3, 3 * 10 ^ i3, 10 ^ i4, 3 * 10 ^ i4,
		10 ^ i5, 3 * 10 ^ i5, 10 ^ i6 ]
	mid <- readFile "/etc/machine-id"
	i <- next "cmp_sorts"
	writeFile ("graph/cmp_sorts" ++ show i ++ ".hason") . (++ "\n") . render
		. ppr $ Dct [
			("machine-id", T . T.pack $ chomp mid),
			("header", L $
				T <$> ["Data.List", "merge", "heap", "quick"]),
			("result", L (uncurry resultToHason <$> rs)) ]

trial :: Int -> IO (Int, Result)
trial n = (n ,) <$> (try =<< mkSample' (0, 10 ^ i8) n)

try :: (Ord a, Bounded a, NFData a) => [a] -> IO Result
try ns = ns `deepseq` do
	evaluate $ rnf ns
	(,,,) <$> tr L.sort <*> tr naturalSort <*> tr heapsort <*> tr quicksort
	where tr f = time (evaluate . rnf $ f ns)

resultToHason :: Int -> Result -> Hason
resultToHason n (dl, m, h, q) = Dct [
	("N", I $ fromIntegral n),
	("time", L $ DT <$> [dl, m, h, q]) ]
