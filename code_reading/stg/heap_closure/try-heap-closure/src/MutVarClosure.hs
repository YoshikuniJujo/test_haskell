{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MutVarClosure (printMutVarClosure) where

import GHC.STRef (STRef(..))
import GHC.IORef (IORef(..), newIORef)
import GHC.Exts.Heap (getClosureData)

printMutVarClosure :: IO ()
printMutVarClosure = do
	r <- newIORef True
	let	!(IORef (STRef mv)) = r
	print =<< getClosureData mv
