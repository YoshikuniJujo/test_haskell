{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Data.Time
import Data.Hason
import Text.PrettyPrint
import Tools

import StraightInsertionSort
import BubbleSort
import StraightSelectionSort

type Result = (NominalDiffTime, NominalDiffTime, NominalDiffTime)

ns :: [Int]
ns = [1000, 2000 .. 8000]

main :: IO ()
main = writeResults =<< trial `mapM` ns

trial :: Int -> IO (Int, Result)
trial n = (n ,) <$> (try =<< mkSample' (0, 10 ^ i8) n)

try :: (Ord a, NFData a) => [a] -> IO Result
try es = es `deepseq` do
	evaluate $ rnf es
	(,,) <$> tr insertionSort <*> tr bubbleSort <*> tr selectionSort
	where tr f = time (evaluate . rnf $ f es)

writeResults :: [(Int, Result)] -> IO ()
writeResults rs = do
	mid <- getMachineId
	fp <- nextFilePath "cmp_on2"
	writeFile fp . (++ "\n") . render . ppr $ Dct [
		("machine-id", T mid),
		("header", L $ T <$> ["insertion", "bubble", "selection"]),
		("result", L (uncurry resultToHason <$> rs)) ]
	where resultToHason n (i, b, s) = Dct
		[("N", I $ fromIntegral n), ("time", L $ DT <$> [i, b, s])]
