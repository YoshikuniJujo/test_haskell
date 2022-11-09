{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
 
module Main (main) where

import Control.Monad
import Data.List (sort)
import Data.Time

import HeapSort.TaocpFree

i3, i4, i5, i6, i7, i8 :: Int
i3 = 3; i4 = 4; i5 = 5; i6 = 6; i7 = 7; i8 = 8

main :: IO ()
main = do
	xs___ <- mkSample' (0, 10 ^ i8) (10 ^ i3)
	print $ last xs___
	showTime "Data.List.sort (10^3)" (10 ^ i3) (print' . last $ sort xs___)
	showTime "heapsort (10^3)      " (10 ^ i3) (print' . last $ heapsort xs___)
	xs___5 <- mkSample' (0, 10 ^ i8) (5 * 10 ^ i3)
	print $ last xs___5
	showTime "Data.List.sort (5 * 10^3)" (5 * 10 ^ i3) (print' . last $ sort xs___5)
	showTime "heapsort (5 * 10^3)      " (5 * 10 ^ i3) (print' . last $ heapsort xs___5)
	xs__ <- mkSample' (0, 10 ^ i8) (10 ^ i4)
	print $ last xs__
	showTime "Data.List.sort (10^4)" (10 ^ i4) (print' . last $ sort xs__)
	showTime "heapsort (10^4)      " (10 ^ i4) (print' . last $ heapsort xs__)
	xs__5 <- mkSample' (0, 10 ^ i8) (5 * 10 ^ i4)
	print $ last xs__5
	showTime "Data.List.sort (5 * 10^4)" (5 * 10 ^ i4) (print' . last $ sort xs__5)
	showTime "heapsort (5 * 10^4)      " (5 * 10 ^ i4) (print' . last $ heapsort xs__5)
	xs_ <- mkSample' (0, 10 ^ i8) (10 ^ i5)
	print $ last xs_
	showTime "Data.List.sort (10^5)" (10 ^ i5) (print' . last $ sort xs_)
	showTime "heapsort (10^5)      " (10 ^ i5) (print' . last $ heapsort xs_)
	xs_3 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i5)
	print $ last xs_3
	showTime "Data.List.sort (3 * 10^5)" (3 * 10 ^ i5) (print' . last $ sort xs_3)
	showTime "heapsort (3 * 10^5)      " (3 * 10 ^ i5) (print' . last $ heapsort xs_3)
	xs_5 <- mkSample' (0, 10 ^ i8) (5 * 10 ^ i5)
	print $ last xs_5
	showTime "Data.List.sort (5 * 10^5)" (5 * 10 ^ i5) (print' . last $ sort xs_5)
	showTime "heapsort (5 * 10^5)      " (5 * 10 ^ i5) (print' . last $ heapsort xs_5)
	xs <- mkSample' (0, 10 ^ i8) (10 ^ i6)
	print $ last xs
	showTime "Data.List.sort (10^6)" (10 ^ i6) (print' . last $ sort xs)
	showTime "heapsort (10^6)      " (10 ^ i6) (print' . last $ heapsort xs)
	{-
	xs' <- mkSample' (0, 10 ^ i8) (2 * 10 ^ i6)
	print $ last xs'
	showTime "Data.List.sort (2 * 10^6)" (2 * 10 ^ i6) (print' . last $ sort xs')
	showTime "heapsort (2 * 10^6)      " (2 * 10 ^ i6) (print' . last $ heapsort xs')
	xs'' <- mkSample' (0, 10 ^ i8) (4 * 10 ^ i6)
	print $ last xs''
	showTime "Data.List.sort (4 * 10^6)" (4 * 10 ^ i6) (print' . last $ sort xs'')
	showTime "heapsort (4 * 10^6)      " (4 * 10 ^ i6) (print' . last $ heapsort xs'')
	-}

nLogN :: Int -> Double
nLogN (fromIntegral -> n) = n * log n

print' :: Show a => a -> IO ()
print' x = putStr $ show x ++ "\t"

showTime :: String -> Int -> IO a -> IO ()
showTime nm n act = do
	putStr $ nm ++ ": "
	t <- time act
	print' t
	print $ realToFrac t / nLogN n * 10 ^ i7

time :: IO a -> IO NominalDiffTime
time act = do
	t0 <- getCurrentTime
	void act
	flip diffUTCTime t0 <$> getCurrentTime
