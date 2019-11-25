{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.Exts.Heap (getClosureData)

import ConstrClosure (number)
import FunClosure (function)
import ThunkBlackholeClosure (printThunkBlackholeConstrClosure)

import IntClosure (printIntClosure)
import FloatClosure (printFloatClosure)

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

	putStrLn "Unboxed Primitive Closures:"
	print =<< getClosureData 123#
	print =<< getClosureData 123.4#

	putStrLn ""
	putStrLn "IntClosure:"
	printIntClosure
	putStrLn ""

	putStrLn "FloatClosure:"
	printFloatClosure
	putStrLn ""
