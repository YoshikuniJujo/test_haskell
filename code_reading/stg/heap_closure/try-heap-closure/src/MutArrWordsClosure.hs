{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MutArrWordsClosure (printMutArrWordsClosure) where

import GHC.Exts.Heap (getClosureData)
import Data.Array.Base (STUArray(..), newListArray)
import Data.Array.IO.Internals (IOUArray(..))

printMutArrWordsClosure :: IO ()
printMutArrWordsClosure = do
	a <- newListArray (0, 5) [5, 4 .. 0] :: IO (IOUArray Int Int)
	let	!(IOUArray (STUArray _ _ _ mba)) = a
	print =<< getClosureData mba
