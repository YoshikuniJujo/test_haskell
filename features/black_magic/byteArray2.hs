{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

import GHC.Prim
import GHC.Types
import GHC.ST
import Data.Primitive

main :: IO ()
main = do
	print $ get -2#
	print $ get -1#
	print $ get 3#
	print $ get 5#
	print $ get 7#
	print $ get 8#
	print $ get 9#

byteArray :: ByteArray
byteArray = runST $ ST $ \s -> let
		(# t, mba #) = newByteArray# 8# s
		u = writeIntArray# mba 3# 123# t
		(# v, ba #) = unsafeFreezeByteArray# mba u in
		(# v, ByteArray ba #)

get :: Int# -> Int
get i = let !(ByteArray ba) = byteArray in I# (indexIntArray# ba i)
