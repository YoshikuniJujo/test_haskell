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

import QuickSort.Wow qualified as Q
import MergeSort.Wow qualified as M

type Result = (NominalDiffTime, NominalDiffTime)

ns :: [Int]
ns = [10 ^ i3, 3 * 10 ^ i3, 10 ^ i4, 3 * 10 ^ i4, 10 ^ i5, 3 * 10 ^ i5, 10 ^ i6]

main :: IO ()
main = writeResults =<< trial `mapM` ns

trial :: Int -> IO (Int, Result)
trial n = (n ,) <$> (try =<< mkSample' (0, 10 ^ i8) n)

try :: (Ord a, NFData a) => [a] -> IO Result
try es = es `deepseq` do
	evaluate $ rnf es
	(,) <$> tr Q.sort <*> tr M.sort
	where tr f = time (evaluate . rnf $ f es)

writeResults :: [(Int, Result)] -> IO ()
writeResults rs = do
	mid <- getMachineId
	fp <- nextFilePath "cmp_wow"
	writeFile fp . (++ "\n") . render . ppr $ Dct [
		("machine-id", T mid),
		("header", L $ T <$> ["quick", "merge"]),
		("result", L (uncurry resultToHason <$> rs)) ]
	where resultToHason n (q, m) = Dct
		[("N", I $ fromIntegral n), ("time", L $ DT <$> [q, m])]
