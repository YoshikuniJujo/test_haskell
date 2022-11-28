{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Data.List qualified as L
import Data.Text qualified as T
import Data.Time
import Data.Hason
import System.Environment
import System.IO
import System.Directory
import Text.PrettyPrint

import QuickSort.Taocp
import MergeSort.Natural
import HeapSort.TaocpFree (heapsort)
import Tools

main :: IO ()
main = do
	args <- getArgs
	let	g = case args of
			[] -> False
			["graph"] -> True
			_ -> error "bad options"
	h <- openFile "/dev/null" WriteMode
	when g $ putStr' h "N\tData.List/merge/heap/quick\n"
	xs_3 <- mkSample' (0, 10 ^ i8) (10 ^ i3)
	r0 <- try h g (10 ^ i3) xs_3
	xs_3_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i3)
	r1 <- try h g (3 * 10 ^ i3) xs_3_5
	xs_4 <- mkSample' (0, 10 ^ i8) (10 ^ i4)
	r2 <- try h g (10 ^ i4) xs_4
	xs_4_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i4)
	r3 <- try h g (3 * 10 ^ i4) xs_4_5
	xs_5 <- mkSample' (0, 10 ^ i8) (10 ^ i5)
	r4 <- try h g (10 ^ i5) xs_5
	xs_5_5 <- mkSample' (0, 10 ^ i8) (3 * 10 ^ i5)
	r5 <- try h g (3 * 10 ^ i5) xs_5_5
	xs_6 <- mkSample' (0, 10 ^ i8) (10 ^ i6)
	r6 <- try h g (10 ^ i6) xs_6
--	xs_7 <- mkSample' (0, 10 ^ i8) (10 ^ i7)
--	try (10 ^ i7) xs_7
	hClose h
	mid <- readFile "/etc/machine-id"
	i' <- next' "cmp_sorts"
	writeFile ("graph/cmp_sorts" ++ show i' ++ ".hason") . (++ "\n") . render . ppr $ Dct [
		("machine-id", T . T.pack $ chomp mid),
		("header", L [T "Data.List", T "merge", T "heap", T "quick"]),
		("result", L [
			resultToHason (10 ^ i3) r0,
			resultToHason (3 * 10 ^ i3) r1,
			resultToHason (10 ^ i4) r2,
			resultToHason (3 * 10 ^ i4) r3,
			resultToHason (10 ^ i5) r4,
			resultToHason (3 * 10 ^ i5) r5,
			resultToHason (10 ^ i6) r6 ]) ]
	print (r0, r1, r2, r3, r4, r5, r6)

chomp :: String -> String
chomp s	| last s == '\n' = init s
	| otherwise = s

type Result = (
	NominalDiffTime, NominalDiffTime, NominalDiffTime, NominalDiffTime )

resultToHason :: Int -> Result -> Hason
resultToHason n (dl, m, h, q) = Dct [
	("N", I $ fromIntegral n),
	("time", L $ DT <$> [dl, m, h, q]) ]

try :: (Ord a, Bounded a, NFData a) => Handle -> Bool -> Int -> [a] -> IO Result
try _ False n ns = ns `deepseq` do
	evaluate $ rnf ns
	(,,,)	<$> showTime ("Data.List.sort "++ show n) n (evaluate . rnf $ L.sort ns)
		<*> showTime ("mergesort      " ++ show n) n (evaluate . rnf $ naturalSort ns)
		<*> showTime ("heapsort       " ++ show n) n (evaluate . rnf $ heapsort ns)
		<*> showTime ("quicksort      " ++ show n) n (evaluate . rnf $ quicksort ns)
try h True n ns = ns `deepseq` do
	evaluate $ rnf ns
	putStr' h $ show n ++ "\t"
	(,,,)
		<$> try1 h L.sort ns
		<*> try1 h naturalSort ns
		<*> try1 h heapsort ns
		<*> try1' h quicksort ns

try1, try1' :: NFData a => Handle -> ([a] -> [a]) -> [a] -> IO NominalDiffTime
try1 h f ns = do
	t <- time (evaluate . rnf $ f ns)
	putStr' h $ show t
	putStr' h "/"
	pure t

try1' h f ns = do
	t <- time (evaluate . rnf $ f ns)
	putStr' h $ show t
	putStr' h "\n"
	pure t

putStr' :: Handle -> String -> IO ()
putStr' h str = do
	hPutStr h str
	putStr str

next' :: String -> IO Int
next' nm = (+ 1) . foldl max 0
	. (read @Int . takeWhile (/= '.') . drop (length nm) <$>)
	. filter ((&&) <$> (nm `L.isPrefixOf`) <*> (".hason" `L.isSuffixOf`)) <$> getDirectoryContents "graph"
