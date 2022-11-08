module Main (main) where

import Control.Monad
import Data.List
import Data.Time

import HeapSort.TaocpFree

main :: IO ()
main = do
	xs__ <- mkSample' (0, 10 ^ 7) (10 ^ 4)
	print $ last xs__
	showTime "Data.List.sort (10^4)" (print . last $ sort xs__)
	showTime "heapsort (10^4)     " (print . last $ heapsort xs__)
	xs_ <- mkSample' (0, 10 ^ 7) (10 ^ 5)
	print $ last xs_
	showTime "Data.List.sort (10^5)" (print . last $ sort xs_)
	showTime "heapsort (10^5)     " (print . last $ heapsort xs_)
	xs <- mkSample' (0, 10 ^ 7) (10 ^ 6)
	print $ last xs
	showTime "Data.List.sort (10^6)" (print . last $ sort xs)
	showTime "heapsort (10^6)     " (print . last $ heapsort xs)
	xs' <- mkSample' (0, 10 ^ 8) (5 * 10 ^ 6)
	print $ last xs'
	showTime "Data.List.sort (5 * 10^6)" (print . last $ sort xs')
	showTime "heapsort (5 * 10^6)     " (print . last $ heapsort xs')

showTime :: String -> IO a -> IO ()
showTime nm act = do
	putStrLn $ nm ++ ": "
	print =<< time act

time :: IO a -> IO NominalDiffTime
time act = do
	t0 <- getCurrentTime
	void act
	flip diffUTCTime t0 <$> getCurrentTime
