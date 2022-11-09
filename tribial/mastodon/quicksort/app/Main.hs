module Main (main) where

import Control.Monad
import Data.List
import Data.Time

import QuickSort.Taocp

main :: IO ()
main = do
	xs <- mkSample' (0, 10 ^ 7) (10 ^ 6)
	print $ last xs
	showTime "Data.List.sort (10^6)" (print' . last $ sort xs)
	showTime "quicksort (10^6)     " (print' . last $ quicksort xs)
	{-
	xs' <- mkSample' (0, 10 ^ 8) (10 ^ 7)
	print $ last xs'
	showTime "Data.List.sort (10^7)" (print' . last $ sort xs')
	showTime "quicksort (10^7)     " (print' . last $ quicksort xs')
	-}

print' :: Show a => a -> IO ()
print' x = putStr $ show x ++ "\t"

showTime :: String -> IO a -> IO ()
showTime nm act = do
	putStr $ nm ++ ":\t"
	print =<< time act

time :: IO a -> IO NominalDiffTime
time act = do
	t0 <- getCurrentTime
	void act
	flip diffUTCTime t0 <$> getCurrentTime
