{-# LANGUAGE MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AddrClosure (printAddrClosure) where

import GHC.Ptr (Ptr(..), nullPtr)
import GHC.Exts.Heap (getClosureData)

printAddrClosure :: IO ()
printAddrClosure = print =<< getClosureData a
	where !(Ptr a) = nullPtr
