{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import CodensityIterateeProblem

main :: IO ()
main = do
--	{-# SCC "RunNotCodensityTransformed" #-} print sample
--	{-# SCC "RunCodensityTransformed" #-} print sampleCo
--	{-# SCC "RunNotCodensityTransformed2" #-} print sample2'
--	{-# SCC "RunCodensityTransformed2" #-} print sample2Co
	{-# SCC "Sample4Co_2000" #-} print $ sample4Co 2000
	{-# SCC "Sample4Co_4000" #-} print $ sample4Co 4000
