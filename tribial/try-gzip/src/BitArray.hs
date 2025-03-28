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

bitArrayToBs :: BitArray -> Either String BS.ByteString
bitArrayToBs = \case
	BitArray { bit0 = 0, bitsLen = ln, bitsBody = bs }
		| ln `mod` 8 == 0 -> Right $ BS.take (ln `div` 8) bs
		| otherwise -> Left "not at byte boundary"
	_ -> Left "not at byte boundary"

splitAt :: Int -> BitArray -> Either String (BitArray, BitArray)
splitAt n BitArray { bit0 = i, bitsLen = ln, bitsBody = bs }
	| n <= ln = Right (
		BitArray { bit0 = i, bitsLen = n, bitsBody = BS.take (n' + 1) bs },
		BitArray { bit0 = i', bitsLen = ln - n, bitsBody = BS.drop n' bs } )
	| otherwise = Left "not enough"
	where (n', i') = (n + i) `divMod` 8

uncons :: BitArray -> Either String (Bool, BitArray)
uncons = \case
	BitArray { bitsLen = 0 } -> Left "empty"
	BitArray { bit0 = 7, bitsLen = ln, bitsBody = bs } -> Right (
		testBit (BS.head bs) 7,
		BitArray { bit0 = 0, bitsLen = ln - 1, bitsBody = BS.tail bs } )
	BitArray { bit0 = i, bitsLen = ln, bitsBody = bs } -> Right (
		testBit (BS.head bs) i,
		BitArray { bit0 = i + 1, bitsLen = ln - 1, bitsBody = bs } )

bitArrayToBools :: BitArray -> [Bool]
bitArrayToBools = L.unfoldr $ either (const Nothing) Just . uncons

splitAtByteBoundary :: BitArray -> Either String (BitArray, BitArray)
splitAtByteBoundary ba@BitArray { bit0 = i } =
	splitAt (8 - ((i - 1) `mod` 8 + 1)) ba

takeByteString :: Int -> BitArray -> Either String BS.ByteString
takeByteString n = \case
	BitArray { bit0 = 0, bitsLen = ln, bitsBody = bs }
		| ln >= n * 8 -> Right $ BS.take n bs
		| otherwise -> Left "not enough"
	_ -> Left "not at byte boundary"

dropByteString :: Int -> BitArray -> Either String BitArray
dropByteString n = \case
	BitArray { bit0 = 0, bitsLen = ln, bitsBody = bs }
		| ln >= n * 8 -> Right $ BitArray {
			bit0 = 0, bitsLen = ln - n * 8,
			bitsBody = BS.drop n bs }
		| otherwise -> Left "not enough"
	_ -> Left "not at byte boundary"

splitAtByteString :: Int -> BitArray -> Either String (BS.ByteString, BitArray)
splitAtByteString n = \case
	BitArray { bit0 = 0, bitsLen = ln, bitsBody = bs }
		| ln >= n * 8 -> Right (t, BitArray {
			bit0 = 0, bitsLen = ln - n * 8, bitsBody = d })
		| otherwise -> Left "not enough"
		where (t, d) = BS.splitAt n bs
	_ -> Left "not at byte boundary"
