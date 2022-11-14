{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Data.List qualified as L
import System.Environment
import System.IO
import System.Directory

import QuickSort.Taocp
import MergeSort
import HeapSort.TaocpFree (heapsort)
import Tools

main :: IO ()
main = do
	args <- getArgs
	i <- next "cmp_sorts"
	h <- openFile ("graph/cmp_sorts" ++ show i ++ ".txt") WriteMode
	let	g = case args of
			[] -> False
			["graph"] -> True
			_ -> error "bad options"
	when g $ putStr' h "N\tData.List/merge/heap/quick\n"
	xs_3 <- mkSample' (0, 10 ^ i8) (10 ^ i3)
	try h g (10 ^ i3) xs_3
	xs_3_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i3)
	try h g (3 * 10 ^ i3) xs_3_5
	xs_4 <- mkSample' (0, 10 ^ i8) (10 ^ i4)
	try h g (10 ^ i4) xs_4
	xs_4_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i4)
	try h g (3 * 10 ^ i4) xs_4_5
	xs_5 <- mkSample' (0, 10 ^ i8) (10 ^ i5)
	try h g (10 ^ i5) xs_5
	xs_5_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i5)
	try h g (3 * 10 ^ i5) xs_5_5
	xs_6 <- mkSample' (0, 10 ^ i8) (10 ^ i6)
	try h g (10 ^ i6) xs_6
--	xs_7 <- mkSample' (0, 10 ^ i8) (10 ^ i7)
--	try (10 ^ i7) xs_7
	hClose h

try :: (Ord a, Bounded a, NFData a) => Handle -> Bool -> Int -> [a] -> IO ()
try _ False n ns = ns `deepseq` do
	evaluate $ rnf ns
	showTime ("Data.List.sort "++ show n) n (evaluate . rnf $ L.sort ns)
	showTime ("mergesort      " ++ show n) n (evaluate . rnf $ naturalSort ns)
	showTime ("heapsort       " ++ show n) n (evaluate . rnf $ heapsort ns)
	showTime ("quicksort      " ++ show n) n (evaluate . rnf $ quicksort ns)
try h True n ns = ns `deepseq` do
	evaluate $ rnf ns
	putStr' h $ show n ++ "\t"
	putStr' h . show =<< time (evaluate . rnf $ L.sort ns); putStr' h "/"
	putStr' h . show =<< time (evaluate . rnf $ naturalSort ns); putStr' h "/"
	putStr' h . show =<< time (evaluate . rnf $ heapsort ns); putStr' h "/"
	putStr' h . show =<< time (evaluate . rnf $ quicksort ns); putStr' h "\n"

putStr' :: Handle -> String -> IO ()
putStr' h str = do
	hPutStr h str
	putStr str

next :: String -> IO Int
next nm = (+ 1) . foldl max 0
	. (read @Int . takeWhile (/= '.') . drop (length nm) <$>)
	. filter (nm `L.isPrefixOf`) <$> getDirectoryContents "graph"
