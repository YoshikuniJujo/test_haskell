{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ThunkBlackholeClosure (printThunkBlackholeConstrClosure) where

import GHC.Exts.Heap (getClosureData)
import Control.Concurrent (threadDelay)

printThunkBlackholeConstrClosure :: FilePath -> IO ()
printThunkBlackholeConstrClosure fp = do
	n <- read <$> readFile fp :: IO Int
	print =<< getClosureData n
	print n
	print =<< getClosureData n
	threadDelay 500000
	print =<< getClosureData n
