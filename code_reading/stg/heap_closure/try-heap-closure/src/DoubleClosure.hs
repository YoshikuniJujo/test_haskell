{-# LANGUAGE MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DoubleClosure (printDoubleClosure) where

import GHC.Types (Double(..))
import GHC.Exts.Heap (getClosureData)

printDoubleClosure :: IO ()
printDoubleClosure = print =<< getClosureData d
	where !(D# d) = 123
