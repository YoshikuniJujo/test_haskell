{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Text.Gunichar where

import Data.Bits
import Data.Word
import Data.Char

#include <pango/pango.h>

sample :: [#{type gunichar}]
sample = fromIntegral . ord <$> "あいうえお"

toUtf8 :: #{type gunichar} -> Maybe [Word8]
toUtf8 n
	| n < 0x80 = Just [fromIntegral n]
	| n < 0x800 = Just $ fromIntegral <$>  [
		0xc0 .|. n `shiftR` 6,
		0x80 .|. n .&. 0x3f ]
	| n < 0x10000 = Just $ fromIntegral <$> [
		0xe0 .|. n `shiftR` 12,
		0x80 .|. n `shiftR` 6 .&. 0x3f,
		0x80 .|. n .&. 0x3f ]
	| n < 0x200000 = Just $ fromIntegral <$> [
		0xf0 .|. n `shiftR` 18,
		0x80 .|. n `shiftR` 12 .&. 0x3f,
		0x80 .|. n `shiftR` 6 .&. 0x3f,
		0x80 .|. n .&. 0x3ef ]
	| otherwise = Nothing
