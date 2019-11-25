{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MVarClosure (printMVarClosure) where

import GHC.MVar (MVar(..), newMVar)
import GHC.Exts.Heap (getClosureData)

printMVarClosure :: IO ()
printMVarClosure = do
	MVar mv <- newMVar True
	print =<< getClosureData mv
