{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.List (sort)

import QuickSort.Taocp
import MergeSort.Natural
import Tools

main :: IO ()
main = do
	xs_ <- mkSample' (0, 10 ^ i8) (2 ^ i16)
	print $ last xs_
	_ <- showTime "Data.List.sort (2^16)" (2 ^ i16) (print' . last $ sort xs_)
	_ <- showTime "quicksort (2^16)     " (2 ^ i16) (print' . last $ quicksort xs_)
	_ <- showTime "mergesort (2^16)     " (2 ^ i16) (print' . last $ naturalSort xs_)
	xs_18 <- mkSample' (0, 10 ^ i8) (2 ^ i18)
	print $ last xs_18
	_ <- showTime "Data.List.sort (2^18)" (2 ^ i18) (print' . last $ sort xs_18)
	_ <- showTime "quicksort (2^18)     " (2 ^ i18) (print' . last $ quicksort xs_18)
	_ <- showTime "mergesort (2^18)     " (2 ^ i18) (print' . last $ naturalSort xs_18)
	xs <- mkSample' (0, 10 ^ i8) (2 ^ i20)
	print $ last xs
	_ <- showTime "Data.List.sort (2^20)" (2 ^ i20) (print' . last $ sort xs)
	_ <- showTime "quicksort (2^20)     " (2 ^ i20) (print' . last $ quicksort xs)
	_ <- showTime "mergesort (2^20)     " (2 ^ i20) (print' . last $ naturalSort xs)
	xs' <- mkSample' (0, 10 ^ i8) (2 ^ i22)
	print $ last xs'
--	showTime "Data.List.sort (2^22)" (2 ^ i22) (print' . last $ sort xs')
	_ <- showTime "quicksort (2^22)     " (2 ^ i22) (print' . last $ quicksort xs')
	_ <- showTime "mergesort (2^22)     " (2 ^ i22) (print' . last $ naturalSort xs')
	xs_23 <- mkSample' (0, 10 ^ i8) (2 ^ i23)
	print $ last xs_23
	_ <- showTime "quicksort (2^23)     " (2 ^ i23) (print' . last $ quicksort xs_23)
	_ <- showTime "mergesort (2^23)     " (2 ^ i23) (print' . last $ naturalSort xs_23)
	pure ()
