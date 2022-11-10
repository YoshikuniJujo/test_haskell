{-# LANGUAGE BlockArguments, ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Foldable
import System.Environment
import System.IO
import System.Directory
import System.Random hiding (next)

import QuickSort.Taocp
import Tools

import Data.List qualified as L

main :: IO ()
main = do
	args <- getArgs
	let	gr = case args of
			[] -> False
			["graph"] -> True
	i <- next
	h <- openFile ("graph/trial" ++ show i ++ ".txt") WriteMode
	mainTrial h gr
	hClose h

next :: IO Int
next = (+ 1) . maximum
	. (read @Int . takeWhile (/= '.') . drop 5 <$>)
	. filter ("trial" `L.isPrefixOf`) <$> getDirectoryContents "graph"

mainTrial :: Handle -> Bool -> IO ()
mainTrial h gr = do
	if gr then pure () else putStrLn "size: 2 ^ 21"
--	sz <- randomRIO (2 ^ i17 - 2 ^ i8, 2 ^ i14)
	xs <- mkSample' (0, 10 ^ i10) (2 ^ i17) -- sz
	readLast xs
	showTime' h gr 1 "quicksort (m = 1)     " (2 ^ i21) (readLast $ quicksortM 1 xs)
	showTime' h gr 2 "quicksort (m = 2)     " (2 ^ i21) (readLast $ quicksortM 2 xs)
	showTime' h gr 3 "quicksort (m = 3)     " (2 ^ i21) (readLast $ quicksortM 3 xs)
	showTime' h gr 4 "quicksort (m = 4)     " (2 ^ i21) (readLast $ quicksortM 4 xs)
	showTime' h gr 5 "quicksort (m = 5)     " (2 ^ i21) (readLast $ quicksortM 5 xs)
	showTime' h gr 6 "quicksort (m = 6)     " (2 ^ i21) (readLast $ quicksortM 6 xs)
	showTime' h gr 7 "quicksort (m = 7)     " (2 ^ i21) (readLast $ quicksortM 7 xs)
	showTime' h gr 8 "quicksort (m = 8)     " (2 ^ i21) (readLast $ quicksortM 8 xs)
	showTime' h gr 9 "quicksort (m = 9)     " (2 ^ i21) (readLast $ quicksortM 9 xs)
	showTime' h gr 10 "quicksort (m = 10)    " (2 ^ i21) (readLast $ quicksortM 10 xs)
	showTime' h gr 11 "quicksort (m = 11)    " (2 ^ i21) (readLast $ quicksortM 11 xs)
	showTime' h gr 12 "quicksort (m = 12)    " (2 ^ i21) (readLast $ quicksortM 12 xs)
	showTime' h gr 13 "quicksort (m = 13)    " (2 ^ i21) (readLast $ quicksortM 13 xs)
	showTime' h gr 14 "quicksort (m = 14)    " (2 ^ i21) (readLast $ quicksortM 14 xs)
	showTime' h gr 15 "quicksort (m = 15)    " (2 ^ i21) (readLast $ quicksortM 15 xs)
	showTime' h gr 16 "quicksort (m = 16)    " (2 ^ i21) (readLast $ quicksortM 16 xs)
	showTime' h gr 17 "quicksort (m = 17)    " (2 ^ i21) (readLast $ quicksortM 17 xs)
	showTime' h gr 18 "quicksort (m = 18)    " (2 ^ i21) (readLast $ quicksortM 18 xs)
	showTime' h gr 27 "quicksort (m = 27)    " (2 ^ i21) (readLast $ quicksortM 27 xs)
	showTime' h gr 36 "quicksort (m = 36)    " (2 ^ i21) (readLast $ quicksortM 36 xs)
	showTime' h gr 128 "quicksort (m = 128)   " (2 ^ i21) (readLast $ quicksortM 128 xs)
	showTime' h gr 256 "quicksort (m = 256)   " (2 ^ i21) (readLast $ quicksortM 256 xs)
--	showTime' h gr 512 "quicksort (m = 512)   " (2 ^ i21) (readLast $ quicksortM 512 xs)
--	showTime' h gr 1024 "quicksort (m = 1024)  " (2 ^ i21) (readLast $ quicksortM 1024 xs)
--	showTime' h gr 2048 "quicksort (m = 2048)  " (2 ^ i21) (readLast $ quicksortM 2048 xs)

readLast :: [Int] -> IO ()
readLast ns = putStr $ replicate (last ns - last ns) 'c'

showTime' :: Handle -> Bool -> Int -> String -> Int -> IO a -> IO ()
showTime' _ False _ lbl n act = showTime lbl n act
showTime' h True m _ _ act = showTimeMGraph h m act
