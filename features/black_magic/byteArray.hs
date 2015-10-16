{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}

import GHC.Prim
import GHC.Types
import GHC.ST
import Data.Primitive

main :: IO ()
main = do
	print $ int -1#
	print $ int 0#
	print $ int 1#
	print $ int 2#
	print $ int 3#
	print $ int 4#

some :: ByteArray
some = runST $ ST hoge

hoge :: State# s -> (# State# s, ByteArray #)
hoge s = case newByteArray# 3# s of
	(# t, mba #) -> case unsafeFreezeByteArray# mba t of
		(# u, ba #) -> (# u, ByteArray ba #)

int :: Int# -> Int
int i = let !(ByteArray ba) = some in indexByteArray# ba i
