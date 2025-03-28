{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BitArray where

import Prelude hiding (splitAt)
import Data.Bits
import Data.List qualified as L
import Data.ByteString qualified as BS

data BitArray =
	BitArray { bit0 :: Int, bitsLen :: Int, bitsBody :: BS.ByteString }
	deriving Show

bsToBitArray :: BS.ByteString -> BitArray
bsToBitArray bs =
	BitArray { bit0 = 0, bitsLen = 8 * BS.length bs, bitsBody = bs }

splitAt :: Int -> BitArray -> Maybe (BitArray, BitArray)
splitAt n BitArray { bit0 = i, bitsLen = ln, bitsBody = bs }
	| n <= ln = Just (
		BitArray { bit0 = i, bitsLen = n, bitsBody = BS.take (n' + 1) bs },
		BitArray { bit0 = i', bitsLen = ln - n, bitsBody = BS.drop n' bs } )
	| otherwise = Nothing
	where (n', i') = (n + i) `divMod` 8

uncons :: BitArray -> Maybe (Bool, BitArray)
uncons = \case
	BitArray { bitsLen = 0 } -> Nothing
	BitArray { bit0 = 7, bitsLen = ln, bitsBody = bs } -> Just (
		testBit (BS.head bs) 7,
		BitArray { bit0 = 0, bitsLen = ln - 1, bitsBody = BS.tail bs } )
	BitArray { bit0 = i, bitsLen = ln, bitsBody = bs } -> Just (
		testBit (BS.head bs) i,
		BitArray { bit0 = i + 1, bitsLen = ln - 1, bitsBody = bs } )

bitArrayToBools :: BitArray -> [Bool]
bitArrayToBools = L.unfoldr uncons

splitAtByteBoundary :: BitArray -> Maybe (BitArray, BitArray)
splitAtByteBoundary ba@BitArray { bit0 = i } =
	splitAt (8 - ((i - 1) `mod` 8 + 1)) ba
