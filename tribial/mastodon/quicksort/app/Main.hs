{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.List
import Data.Time

import QuickSort.Taocp

i5, i6, i7, i8, i16, i17, i18, i19, i20, i21, i22, i23 :: Int
i5 = 5; i6 = 6; i7 = 7; i8 = 8
i16 = 16; i17 = 17; i18 = 18; i19 = 19; i20 = 20; i21 = 21; i22 = 22; i23 = 23

main :: IO ()
main = do
	xs_ <- mkSample' (0, 10 ^ i8) (2 ^ i16)
	print $ last xs_
	showTime "Data.List.sort (2^16)" (2 ^ i16) (print' . last $ sort xs_)
	showTime "quicksort (2^16)     " (2 ^ i16) (print' . last $ quicksort xs_)
	xs_18 <- mkSample' (0, 10 ^ i8) (2 ^ i18)
	print $ last xs_18
	showTime "Data.List.sort (2^18)" (2 ^ i18) (print' . last $ sort xs_18)
	showTime "quicksort (2^18)     " (2 ^ i18) (print' . last $ quicksort xs_18)
	xs <- mkSample' (0, 10 ^ i8) (2 ^ i20)
	print $ last xs
	showTime "Data.List.sort (2^20)" (2 ^ i20) (print' . last $ sort xs)
	showTime "quicksort (2^20)     " (2 ^ i20) (print' . last $ quicksort xs)
	xs' <- mkSample' (0, 10 ^ i8) (2 ^ i22)
	print $ last xs'
	showTime "Data.List.sort (2^22)" (2 ^ i22) (print' . last $ sort xs')
	showTime "quicksort (2^22)     " (2 ^ i22) (print' . last $ quicksort xs')
	xs_23 <- mkSample' (0, 10 ^ i8) (2 ^ i23)
	print $ last xs_23
	showTime "quicksort (2^23)     " (2 ^ i23) (print' . last $ quicksort xs_23)

print' :: Show a => a -> IO ()
print' x = putStr $ show x ++ "\t"

nLogN :: Int -> Double
nLogN (fromIntegral -> n) = n * log n

showTime :: String -> Int -> IO a -> IO ()
showTime nm n act = do
	putStr $ nm ++ ":\t"
	t <- time act
	print' t
	print $ realToFrac t / nLogN n * 10 ^ i7

time :: IO a -> IO NominalDiffTime
time act = do
	t0 <- getCurrentTime
	void act
	flip diffUTCTime t0 <$> getCurrentTime
