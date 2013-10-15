{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

-- ghc unboxed.hs -rtsopts
-- time ./unboxed +RTS -K400000000 -RTS 200000000 boxed
-- 3.1415926035898605
-- 9.19s user 0.90s system 91% cpu 11.048 total
-- time ./unboxed +RTS -K400000000 -RTS 200000000 unboxed
-- 0.40s user 0.00s system 98% cpu 0.401 total

import GHC.Prim
import GHC.Types
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	let	i = read $ head args
		!(W# ui) = i
	case tail args of
		"boxed" : _ -> print $ 4 * getPi4 0 1 i
		"unboxed" : _ -> print $ 4 * (D# (getPi4' 0.0## 1.0## ui))
		_ -> error "bad arguments"

getPi4 :: Double -> Double -> Word -> Double
getPi4 p _ 0 = p
getPi4 p i n = getPi4 (p + recip i) (negate $ i + signum i * 2) (n - 1)

getPi4' :: Double# -> Double# -> Word# -> Double#
getPi4' p _ 0## = p
getPi4' p i n = getPi4' (p +## (1.0## /## i))
	(negateDouble# (i +## (signumDouble# i *## 2.0##))) (n `minusWord#` 1##)

signumDouble# :: Double# -> Double#
signumDouble# x = let (# s, _, _, _ #) = decodeDouble_2Int# x in int2Double# s
