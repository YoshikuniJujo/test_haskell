{-# LANGUAGE MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Int64Closure (printInt64Closure) where

import GHC.Int (Int64(..))
import GHC.Exts.Heap (getClosureData)

printInt64Closure :: IO ()
printInt64Closure = print =<< getClosureData i
	where !(I64# i) = 123
