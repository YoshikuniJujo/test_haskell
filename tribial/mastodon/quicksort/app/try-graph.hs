{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Data.List qualified as L
import System.Environment

import QuickSort.Taocp
import MergeSort
import Tools

main :: IO ()
main = do
	args <- getArgs
	let	g = case args of
			[] -> False
			["graph"] -> True
			_ -> error "bad options"
	when g $ putStrLn "N\tData.List/quick/merge"
	xs_3 <- mkSample' (0, 10 ^ i8) (10 ^ i3)
	try g (10 ^ i3) xs_3
	xs_3_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i3)
	try g (3 * 10 ^ i3) xs_3_5
	xs_4 <- mkSample' (0, 10 ^ i8) (10 ^ i4)
	try g (10 ^ i4) xs_4
	xs_4_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i4)
	try g (3 * 10 ^ i4) xs_4_5
	xs_5 <- mkSample' (0, 10 ^ i8) (10 ^ i5)
	try g (10 ^ i5) xs_5
	xs_5_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i5)
	try g (3 * 10 ^ i5) xs_5_5
	xs_6 <- mkSample' (0, 10 ^ i8) (10 ^ i6)
	try g (10 ^ i6) xs_6
--	xs_7 <- mkSample' (0, 10 ^ i8) (10 ^ i7)
--	try (10 ^ i7) xs_7

try :: Bool -> Int -> [Int] -> IO ()
try False n ns = ns `deepseq` do
	evaluate $ rnf ns
	showTime ("Data.List.sort "++ show n) n (evaluate . rnf $ L.sort ns)
	showTime ("quicksort      " ++ show n) n (evaluate . rnf $ quicksort ns)
	showTime ("mergesort      " ++ show n) n (evaluate . rnf $ naturalSort ns)
try True n ns = ns `deepseq` do
	evaluate $ rnf ns
	putStr $ show n ++ "\t"
	putStr . show =<< time (evaluate . rnf $ L.sort ns); putStr "/"
	putStr . show =<< time (evaluate . rnf $ quicksort ns); putStr "/"
	putStr . show =<< time (evaluate . rnf $ naturalSort ns); putStr "\n"
