{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Word
import Data.Bits
import Data.Char
import Numeric

abcd0 :: (Word32, Word32, Word32, Word32)
abcd0 = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476)

type Function = Word32 -> Word32 -> Word32 -> Word32

fs :: [Function]
fs = concatMap (replicate 16) [f, g, h, i]
	where
	f x y z = x .&. y .|. complement x .&. z
	g x y z = x .&. z .|. y .&. complement z
	h x y z = x `xor` y `xor` z
	i x y z = y `xor` (x .|. complement z)

ts :: [Word32]
ts = map (floor . ((4294967296 :: Double) *) . abs . sin) [1 .. 64]

ks, ss :: [Int]
ks = [0..15] ++
	take 16 (iterate ((`mod` 16) . (+ 5)) 1) ++
	take 16 (iterate ((`mod` 16) . (+ 3)) 5) ++
	take 16 (iterate ((`mod` 16) . (+ 7)) 0)
ss = concatMap (concat . replicate 4) [
	[7, 12, 17, 22],
	[5, 9,  14, 20],
	[4, 11, 16, 23],
	[6, 10, 15, 21] ]

step :: [Word32] -> (Word32, Word32, Word32, Word32) ->
	(Function, Int, Int, Word32) -> (Word32, Word32, Word32, Word32)
step xs (a, b, c, d) (f, k, s, t) = (d, a', b, c)
	where a' = b + (a + f b c d + xs !! k + t) `rotateL` s

steps :: (Word32, Word32, Word32, Word32) ->
	[Word32] -> (Word32, Word32, Word32, Word32)
steps (a, b, c, d) xs = (a + a', b + b', c + c', d + d')
	where (a', b', c', d') = foldl (step xs) (a, b, c, d) $ zip4 fs ks ss ts

md5sum :: String -> String
md5sum dat = concatMap (tos 4) [a, b, c, d]
	where
	(a, b, c, d) = foldl steps abcd0 $ ppr dat
	tos (0 :: Int) _ = ""
	tos i n = h2 (n .&. 0xff) ++ tos (i - 1) (n `shiftR` 8)
	h2 n = let s = showHex n "" in replicate (2 - length s) '0' ++ s

ppr :: String -> [[Word32]]
ppr = unfoldr (uc 16) . map tw . unfoldr (uc 4) . padd
	where
	tw [] = 0
	tw (c : cs) = fromIntegral (ord c) .|. tw cs `shiftL` 8
	uc _ [] = Nothing
	uc n xs = Just (take n xs, drop n xs)

padd :: String -> String
padd s = s ++ "\x80" ++
	((55 - l `mod` 64) `mod` 64) `replicate` '\x00' ++ lte 8 (8 * l)
	where
	l = length s
	lte (0 :: Int) _ = ""
	lte i n = chr (n .&. 0xff) : lte (i - 1) (n `shiftR` 8)
