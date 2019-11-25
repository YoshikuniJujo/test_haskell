{-# LANGUAGE MagicHash, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ArrWordsClosure (printArrWordsClosure) where

import GHC.Integer.GMP.Internals (Integer(..), BigNat(..))
import GHC.Exts.Heap (getClosureData)

printArrWordsClosure :: IO ()
printArrWordsClosure = print =<< getClosureData bn
	where !(Jp# (BN# bn)) = 12345678901234567890
