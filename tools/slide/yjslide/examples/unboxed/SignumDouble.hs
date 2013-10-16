{-# LANGUAGE MagicHash, UnboxedTuples #-}

module SignumDouble where

import GHC.Prim

signumDouble# :: Double# -> Double#
signumDouble# x = let (# s, _, _, _ #) = decodeDouble_2Int# x in int2Double# s
