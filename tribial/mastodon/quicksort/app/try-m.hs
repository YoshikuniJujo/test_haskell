{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import QuickSort.Taocp
import Tools

main :: IO ()
main = do
	putStrLn "size: 2 ^ 21"
	xs <- mkSample' (0, 10 ^ i10) (2 ^ i21)
	print $ last xs
	showTime "quicksort (m = 1)     " (2 ^ i21) (print' . last $ quicksortM 1 xs)
	showTime "quicksort (m = 2)     " (2 ^ i21) (print' . last $ quicksortM 2 xs)
	showTime "quicksort (m = 3)     " (2 ^ i21) (print' . last $ quicksortM 3 xs)
	showTime "quicksort (m = 4)     " (2 ^ i21) (print' . last $ quicksortM 4 xs)
	showTime "quicksort (m = 5)     " (2 ^ i21) (print' . last $ quicksortM 5 xs)
	showTime "quicksort (m = 6)     " (2 ^ i21) (print' . last $ quicksortM 6 xs)
	showTime "quicksort (m = 7)     " (2 ^ i21) (print' . last $ quicksortM 7 xs)
	showTime "quicksort (m = 8)     " (2 ^ i21) (print' . last $ quicksortM 8 xs)
	showTime "quicksort (m = 9)     " (2 ^ i21) (print' . last $ quicksortM 9 xs)
	showTime "quicksort (m = 10)    " (2 ^ i21) (print' . last $ quicksortM 10 xs)
	showTime "quicksort (m = 11)    " (2 ^ i21) (print' . last $ quicksortM 11 xs)
	showTime "quicksort (m = 12)    " (2 ^ i21) (print' . last $ quicksortM 12 xs)
	showTime "quicksort (m = 13)    " (2 ^ i21) (print' . last $ quicksortM 13 xs)
	showTime "quicksort (m = 14)    " (2 ^ i21) (print' . last $ quicksortM 14 xs)
	showTime "quicksort (m = 15)    " (2 ^ i21) (print' . last $ quicksortM 15 xs)
	showTime "quicksort (m = 16)    " (2 ^ i21) (print' . last $ quicksortM 16 xs)
	showTime "quicksort (m = 17)    " (2 ^ i21) (print' . last $ quicksortM 17 xs)
	showTime "quicksort (m = 18)    " (2 ^ i21) (print' . last $ quicksortM 18 xs)
	showTime "quicksort (m = 27)    " (2 ^ i21) (print' . last $ quicksortM 27 xs)
	showTime "quicksort (m = 36)    " (2 ^ i21) (print' . last $ quicksortM 36 xs)
	showTime "quicksort (m = 128)   " (2 ^ i21) (print' . last $ quicksortM 128 xs)
	showTime "quicksort (m = 256)   " (2 ^ i21) (print' . last $ quicksortM 256 xs)
	showTime "quicksort (m = 512)   " (2 ^ i21) (print' . last $ quicksortM 512 xs)
	showTime "quicksort (m = 1024)  " (2 ^ i21) (print' . last $ quicksortM 1024 xs)
	showTime "quicksort (m = 2048)  " (2 ^ i21) (print' . last $ quicksortM 2048 xs)
