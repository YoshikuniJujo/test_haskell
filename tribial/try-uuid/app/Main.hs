{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.UUID.V4
import Data.Time

n :: Int
n = 10 ^ (6 :: Int)

main :: IO ()
main = do
	t <- getCurrentTime
	print . last =<< replicateM n nextRandom
	t' <- getCurrentTime
	let	d = t' `diffUTCTime` t
	print d

	putStrLn $
		show @Double (realToFrac (d / (16 * fromIntegral n)) *
			10 ^ (9 :: Int)) ++ "ns"
	putStrLn $
		show @Double (16 * fromIntegral n / realToFrac d /
			10 ^ (6 :: Int) * 8) ++ "Mbps"
	putStrLn $
		show @Double (16 * fromIntegral n / realToFrac d /
			10 ^ (6 :: Int) * 8 * 40) ++ "Mbps"
