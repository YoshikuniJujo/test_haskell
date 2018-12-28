{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Control.Concurrent
import Data.Fixed
import Data.Time
import System.IO

main :: IO ()
main = do
	t0 <- getCurrentTime
	count t0

count :: UTCTime -> IO ()
count t0 = lp
	where
	lp = do
		threadDelay 90000
		t1 <- getCurrentTime
		let	s = showDiff $ diffUTCTime t1 t0
		putStr s
		putStr $ replicate (length s) '\b'
		hFlush stdout
		lp

showDiff :: NominalDiffTime -> String
showDiff s = let
	(m, s') =  s `dm` 60
	mstr = show (round m :: Int)
	sstr = show (fromRational $ toRational s' :: Centi) in
	replicate (2 - length mstr) '0' ++ mstr ++ ":" ++ sstr

dm :: (Num n, Ord n) => n -> n -> (n, n)
dm a b	| a >= b = (1 +) `first` dm (a - b) b
	| otherwise = (0, a)
