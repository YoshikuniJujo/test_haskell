{-# LANGUAGE MultiParamTypeClasses #-}

module Pack (
	Pack(..)
) where

import Data.Word
import Data.Char
import Data.Bits

class Pack c e where
	pack :: [e] -> [c]
	unpack :: [c] -> [e]
	index :: [c] -> Int -> e

instance Pack Word8 Bool where
	pack [] = []
	pack bs = boolToWord (take 8 bs) : pack (drop 8 bs)
	unpack = concatMap (wordToBool 8)
	index = indexWB 8

instance Pack Char Bool where
	pack = map (chr . fromIntegral) . (pack :: [Bool] -> [Word8])
	unpack = (unpack :: [Word8] -> [Bool]) . map (fromIntegral . ord)
	index = indexWB 8 . map ord

instance Pack Word64 Bool where
	pack [] = []
	pack bs = boolToWord (take 64 bs) : pack (drop 64 bs)
	unpack = concatMap (wordToBool 64)
	index = indexWB 64

boolToWord :: (Bits w, Integral w) => [Bool] -> w
boolToWord (b : bs) = fromIntegral (fromEnum b) .|. (boolToWord bs) `shiftL` 1
boolToWord [] = 0

wordToBool :: (Bits w, Integral w) => Int -> w -> [Bool]
wordToBool s w = bs ++ replicate (s - length bs) False
	where
	bs = wtb w
	wtb 0 = []
	wtb w' = toEnum (fromIntegral (w' .&. 1)) : wtb (w' `shiftR` 1)

indexWB :: (Bits w, Num w) => Int -> [w] -> Int -> Bool
indexWB s (w : ws) i
	| i < s = w .&. (1 `shiftL` i) /= 0
	| otherwise = indexWB s ws (i - s)
