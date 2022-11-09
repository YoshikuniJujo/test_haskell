{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import System.Environment

import QuickSort.Taocp
import Tools

main :: IO ()
main = do
	args <- getArgs
	let	gr = case args of
			[] -> False
			["graph"] -> True
	if gr then pure () else putStrLn "size: 2 ^ 21"
	xs <- mkSample' (0, 10 ^ i10) (2 ^ i21)
	readLast xs
	showTime' gr 1 "quicksort (m = 1)     " (2 ^ i21) (readLast $ quicksortM 1 xs)
	showTime' gr 2 "quicksort (m = 2)     " (2 ^ i21) (readLast $ quicksortM 2 xs)
	showTime' gr 3 "quicksort (m = 3)     " (2 ^ i21) (readLast $ quicksortM 3 xs)
	showTime' gr 4 "quicksort (m = 4)     " (2 ^ i21) (readLast $ quicksortM 4 xs)
	showTime' gr 5 "quicksort (m = 5)     " (2 ^ i21) (readLast $ quicksortM 5 xs)
	showTime' gr 6 "quicksort (m = 6)     " (2 ^ i21) (readLast $ quicksortM 6 xs)
	showTime' gr 7 "quicksort (m = 7)     " (2 ^ i21) (readLast $ quicksortM 7 xs)
	showTime' gr 8 "quicksort (m = 8)     " (2 ^ i21) (readLast $ quicksortM 8 xs)
	showTime' gr 9 "quicksort (m = 9)     " (2 ^ i21) (readLast $ quicksortM 9 xs)
	showTime' gr 10 "quicksort (m = 10)    " (2 ^ i21) (readLast $ quicksortM 10 xs)
	showTime' gr 11 "quicksort (m = 11)    " (2 ^ i21) (readLast $ quicksortM 11 xs)
	showTime' gr 12 "quicksort (m = 12)    " (2 ^ i21) (readLast $ quicksortM 12 xs)
	showTime' gr 13 "quicksort (m = 13)    " (2 ^ i21) (readLast $ quicksortM 13 xs)
	showTime' gr 14 "quicksort (m = 14)    " (2 ^ i21) (readLast $ quicksortM 14 xs)
	showTime' gr 15 "quicksort (m = 15)    " (2 ^ i21) (readLast $ quicksortM 15 xs)
	showTime' gr 16 "quicksort (m = 16)    " (2 ^ i21) (readLast $ quicksortM 16 xs)
	showTime' gr 17 "quicksort (m = 17)    " (2 ^ i21) (readLast $ quicksortM 17 xs)
	showTime' gr 18 "quicksort (m = 18)    " (2 ^ i21) (readLast $ quicksortM 18 xs)
	showTime' gr 27 "quicksort (m = 27)    " (2 ^ i21) (readLast $ quicksortM 27 xs)
	showTime' gr 36 "quicksort (m = 36)    " (2 ^ i21) (readLast $ quicksortM 36 xs)
	showTime' gr 128 "quicksort (m = 128)   " (2 ^ i21) (readLast $ quicksortM 128 xs)
	showTime' gr 256 "quicksort (m = 256)   " (2 ^ i21) (readLast $ quicksortM 256 xs)
	showTime' gr 512 "quicksort (m = 512)   " (2 ^ i21) (readLast $ quicksortM 512 xs)
	showTime' gr 1024 "quicksort (m = 1024)  " (2 ^ i21) (readLast $ quicksortM 1024 xs)
	showTime' gr 2048 "quicksort (m = 2048)  " (2 ^ i21) (readLast $ quicksortM 2048 xs)

readLast :: [Int] -> IO ()
readLast ns = putStr $ replicate (last ns - last ns) 'c'

showTime' :: Bool -> Int -> String -> Int -> IO a -> IO ()
showTime' False _ lbl n act = showTime lbl n act
showTime' True m _ _ act = showTimeMGraph m act
