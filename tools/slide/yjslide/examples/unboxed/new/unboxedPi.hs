{-# LANGUAGE MagicHash, BangPatterns #-}

import Data.Word
import GHC.Prim
import GHC.Types
import Tools

times :: Word
times = 10 ^ 8

main :: IO ()
main = let !(W# t) = times in print $ 4 * (D# (getPi4 0.0## 1.0## t))

getPi4 :: Double# -> Double# -> Word# -> Double#
getPi4 p _ 0## = p
getPi4 p i n = getPi4 (p +## (1.0## /## i))
	(negateDouble# (i +## (signumDouble# i *## 2.0##))) (n `minusWord#` 1##)
