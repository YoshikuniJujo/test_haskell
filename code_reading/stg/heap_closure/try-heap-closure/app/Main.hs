{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Exts.Heap (getClosureData)

import ConstrClosure (number)
import FunClosure (function)
import ThunkBlackholeClosure (printThunkBlackholeConstrClosure)

import IntClosure (printIntClosure)
import FloatClosure (printFloatClosure)
import WordClosure (printWordClosure)
import Int64Closure (printInt64Closure)
import AddrClosure (printAddrClosure)
import MutVarClosure (printMutVarClosure)
import MVarClosure (printMVarClosure)
import ArrWordsClosure (printArrWordsClosure)
import MutArrWordsClosure (printMutArrWordsClosure)
import DoubleClosure (printDoubleClosure)

main :: IO ()
main = do
	putStrLn "ConstrClosure:"
	print =<< getClosureData number
	putStrLn ""

	putStrLn "FunClosure:"
	print =<< getClosureData function
	putStrLn ""

	putStrLn "ThunkClosure, BlackholeClosure:"
	printThunkBlackholeConstrClosure "number.txt"
	putStrLn ""

	putStrLn "IntClosure:"
	printIntClosure
	putStrLn ""

	putStrLn "FloatClosure:"
	printFloatClosure
	putStrLn ""

	putStrLn "WordClosure:"
	printWordClosure
	putStrLn ""

	putStrLn "Int64Closure:"
	printInt64Closure
	putStrLn ""

	putStrLn "AddrClosure:"
	printAddrClosure
	putStrLn ""

	putStrLn "MutVarClosure:"
	printMutVarClosure
	putStrLn ""

	putStrLn "MVarClosure:"
	printMVarClosure
	putStrLn ""

	putStrLn "ArrWordsClosure:"
	printArrWordsClosure
	putStrLn ""

	putStrLn "MutArrWordsClosure:"
	printMutArrWordsClosure
	putStrLn ""

	putStrLn "DoubleClosure:"
	printDoubleClosure
	putStrLn ""
