import Data.List
import Data.Word
import Data.Bits
import Data.Char
import Numeric

padding :: String -> String
padding s =
	s ++ "\x80" ++ replicate ((55 - l `mod` 64) `mod` 64) '\x00' ++ len64 8 (8 * l)
	where
	l = length s

len64 :: Int -> Int -> String
len64 0 _ = ""
len64 i n = chr (n `mod` 0x100) : len64 (i - 1) (n `div` 0x100)

a0, b0, c0, d0 :: Word32
a0 = 0x67452301
b0 = 0xefcdab89
c0 = 0x98badcfe
d0 = 0x10325476

funF, funG, funH, funI :: Word32 -> Word32 -> Word32 -> Word32
funF x y z = x .&. y .|. complement x .&. z
funG x y z = x .&. z .|. y .&. complement z
funH x y z = x `xor` y `xor` z
funI x y z = y `xor` (x .|. complement z)

t :: Double -> Word32
t i = floor $ 4294967296 * abs (sin i)

ts :: [Word32]
ts = map t [0 .. 64]

toWord :: [Char] -> Word32
toWord [] = 0
toWord (c : cs) = fromIntegral (ord c) + 0x100 * toWord cs

uncons :: Int -> [a] -> Maybe ([a], [a])
uncons _ [] = Nothing
uncons n xs = Just (take n xs, drop n xs)

fs :: [Word32 -> Word32 -> Word32 -> Word32]
fs = concatMap (replicate 16) [funF, funG, funH, funI]

ks, is, ss :: [Int]
ks = [0..15] ++
	[1, 6 .. 15] ++ [0, 5 .. 15] ++ [4, 9 .. 15] ++
	[3, 8 .. 15] ++ [2, 7 .. 15] ++
	[5, 8 .. 15] ++ [1, 4 .. 15] ++ [0, 3 .. 15] ++ [2] ++
	[0, 7 .. 15] ++ [5, 12 .. 15] ++ [3, 10 .. 15] ++
	[1, 8 .. 15] ++ [6, 13 .. 15] ++ [4, 11 .. 15] ++ [2, 9 .. 15]
ss = concatMap (concat . replicate 4) [
	[7, 12, 17, 22],
	[5, 9, 14, 20],
	[4, 11, 16, 23],
	[6, 10, 15, 21] ]
is = [1 .. 64]

step :: [Word32] -> (Word32, Word32, Word32, Word32)
	-> (Word32 -> Word32 -> Word32 -> Word32, Int, Int, Int)
	-> (Word32, Word32, Word32, Word32)
step xs (a, b, c, d) (j, k, i, s) = (d, a', b, c)
	where
	a' = b + (a + j b c d + xs !! k + ts !! i) `rotateL` s

littleE :: Int -> Word32 -> String
littleE 0 _ = ""
littleE i n = showHex2 (n `mod` 0x100) ++ littleE (i - 1) (n `div` 0x100)

showHex2 :: Word32 -> String
showHex2 n = let s = showHex n "" in replicate (2 - length s) '0' ++ s

sampleMd5 :: String
sampleMd5 = concatMap (littleE 4) [(a + a0), (b + b0), (c + c0), (d + d0)]
	where
	(a, b, c, d) = foldl (step sampleXs) (a0, b0, c0, d0) $ zip4 fs ks is ss

steps :: (Word32, Word32, Word32, Word32) ->
	[Word32] -> (Word32, Word32, Word32, Word32)
steps (a, b, c, d) xs = (a + a', b + b', c + c', d + d')
	where
	(a', b', c', d') = foldl (step xs) (a, b, c, d) $ zip4 fs ks is ss

test :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
test (a, b, c, d) = (d, a, b, c)

sampleXs :: [Word32]
sampleXs = map toWord . unfoldr (uncons 4) $ padding ""

process :: String -> [[Word32]]
process = unfoldr (uncons 16) . map toWord . unfoldr (uncons 4) . padding

some :: String
some = "a\128\NUL\NUL\NUL" ++ replicate 49 '\NUL' ++ "\b" ++ replicate 7 '\NUL'

md5sum :: String -> String
md5sum s = concatMap (littleE 4) [a, b, c, d]
	where
	(a, b, c, d) = foldl steps (a0, b0, c0, d0) $ process s
