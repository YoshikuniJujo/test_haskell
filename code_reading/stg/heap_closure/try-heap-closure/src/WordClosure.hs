{-# LANGUAGE MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module WordClosure (printWordClosure) where

import GHC.Word (Word(..))
import GHC.Exts.Heap (getClosureData)

printWordClosure :: IO ()
printWordClosure = print =<< getClosureData w
	where !(W# w) = 123
