{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Traversable
import Data.Text qualified as T
import Data.Time
import Data.Hason
import System.Environment
import System.IO
import System.Directory
import System.Random hiding (next)
import Text.PrettyPrint

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
	mainTrial i h gr
	hClose h

next :: IO Int
next = (+ 1) . foldl max 0
	. (read @Int . takeWhile (/= '.') . drop 5 <$>)
	. filter ("trial" `L.isPrefixOf`) <$> getDirectoryContents "graph"

mainTrial :: Int -> Handle -> Bool -> IO ()
mainTrial i h gr = do
	mid <- readFile "/etc/machine-id"
	if gr then pure () else putStrLn "size: 2 ^ 21"
	sz <- randomRIO (13 * 10 ^ i4, 2 ^ i17)
	xs <- mkSample' (0, 10 ^ i10) sz
--	xs <- mkSample' (0, 10 ^ i10) (2 ^ i17) -- sz
--	xs <- mkSample' (0, 10 ^ i10) (10 ^ i5) -- sz
--	xs <- mkSample' (0, 10 ^ i10) (13 * 10 ^ i4) -- sz
	readLast xs
	rslt <- for (
		[1 .. 16] ++ [19, 22 .. 31] ++
		[36, 43 .. 57] ++ [66, 91, 128, 181, 256] ) \m -> do
		t <- showTime' h gr m (readLast $ quicksortM m xs)
		pure $ Dct [
			("M", I $ fromIntegral m),
			("time", DT t) ]
	writeFile ("graph/try-m" ++ show i ++ ".hason") . (++ "\n") . render . ppr $ Dct [
		("machine-id", T . T.pack $ tail mid),
		("result", L rslt)
		]

readLast :: [Int] -> IO ()
readLast ns = putStr $ replicate (last ns - last ns) 'c'

showTime' :: Handle -> Bool -> Int -> IO a -> IO NominalDiffTime
showTime' h _ m act = showTimeMGraph h m act
