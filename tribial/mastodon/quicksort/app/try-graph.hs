{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Data.List qualified as L
import Data.Time
import Data.Hason
import Text.PrettyPrint

import QuickSort.Taocp
import MergeSort.Natural
import HeapSort.TaocpFree (heapsort)
import Tools

type Result = (
	NominalDiffTime, NominalDiffTime, NominalDiffTime, NominalDiffTime )

ns :: [Int]
ns = [10 ^ i3, 3 * 10 ^ i3, 10 ^ i4, 3 * 10 ^ i4, 10 ^ i5, 3 * 10 ^ i5, 10 ^ i6]

main :: IO ()
main = writeResults =<< trial `mapM` ns

trial :: Int -> IO (Int, Result)
trial n = (n ,) <$> (try =<< mkSample' (0, 10 ^ i8) n)

try :: (Ord a, Bounded a, NFData a) => [a] -> IO Result
try es = es `deepseq` do
	evaluate $ rnf es
	(,,,) <$> tr L.sort <*> tr naturalSort <*> tr heapsort <*> tr quicksort
	where tr f = time (evaluate . rnf $ f es)

writeResults :: [(Int, Result)] -> IO ()
writeResults rs = do
	mid <- getMachineId
	fp <- nextFilePath "cmp_sorts"
	writeFile fp . (++ "\n") . render . ppr $ Dct [
		("machine-id", T mid),
		("header", L $ T <$> ["Data.List", "merge", "heap", "quick"]),
		("result", L (uncurry resultToHason <$> rs)) ]
	where resultToHason n (dl, m, h, q) = Dct
		[("N", I $ fromIntegral n), ("time", L $ DT <$> [dl, m, h, q])]
