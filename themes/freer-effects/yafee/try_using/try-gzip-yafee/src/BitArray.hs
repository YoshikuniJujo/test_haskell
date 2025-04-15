{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BitArray (

	-- * BIT ARRAY

	B, empty,

	-- * BIT

	Bit(..), bitsToNum,

	-- * FROM/TO BYTE STRING

	fromByteString, toByteString,

	-- * APPEND

	append, appendByteString,

	-- * SPLIT

	pop, splitAt,

	byteBoundary,

	-- * CONVERT

	toWord8

	) where
-- module BitArray where

import Prelude hiding (take, splitAt)

import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

data B = B { bit0 :: Int, bitsLen :: Int, bitsBody :: BS.ByteString }
	deriving Show

data Bit = O | I deriving (Show, Eq, Ord)

empty :: B
empty = B { bit0 = 0, bitsLen = 0, bitsBody = "" }

pop :: B -> Maybe (Bit, B)
pop B { bit0 = i0, bitsLen = ln, bitsBody = bs } =
	case (i0, ln) of
		(_, 0) -> Nothing
		(7, _) -> case BS.uncons bs of
			Just (b, bs') -> Just (
				bool O I (b `testBit` 7), B {
					bit0 = 0, bitsLen = ln - 1,
					bitsBody = bs' } )
			Nothing -> error "never occur"
		_ -> Just (	bool O I b, B {
					bit0 = i0 + 1, bitsLen = ln - 1,
					bitsBody = bs } )
			where b = BS.head bs `testBit` i0

splitAt :: Int -> B -> Maybe (B, B)
splitAt n (B i ln bs)
	| ln < n = Nothing
	| otherwise = Just (
		normalize $ B i n bs,
		normalize $ B (i + n) (ln - n) bs )

appendByteString :: B -> BS.ByteString -> B
appendByteString
	B { bit0 = i0, bitsLen = ln, bitsBody = bs } bs' =
	B {
		bit0 = i0, bitsLen = ln + 8 * BS.length bs',
		bitsBody = bs `BS.append` bs' }

append :: B -> B -> B
append	B { bit0 = i1, bitsLen = ln1, bitsBody = bs1 }
	B { bit0 = i2, bitsLen = ln2, bitsBody = bs2 } =
	if (i1 + ln1 + i2) `mod` 8 == 0
	then B {
		bit0 = i1, bitsLen = ln1 + ln2, bitsBody = bs1 `BS.append` bs2 }
	else error "yet"

normalize :: B -> B
normalize (B i ln bs)
	| 0 <= i = B i' ln
		. BS.take t $ BS.drop (i `div` 8) bs
	| otherwise = error "bad"
	where
	i' = i `mod` 8
	t = (ln + i' - 1) `div` 8 + 1

toWord8 :: B -> Maybe Word8
toWord8 (B i ln bs)
	| ln + i <= 8 = Just $
		BS.head bs `shiftR` i .&. foldl setBit zeroBits [0 .. ln - 1]
	| ln <= 8 = Just let b0 = BS.head bs; b1 = BS.head $ BS.tail bs in
		b0 `shiftR` i .|.
		b1 `shiftL` (8 - i) .&. foldl setBit zeroBits [0 .. ln - 1]
	| otherwise = Nothing

byteBoundary :: B -> Either (B, BS.ByteString) BS.ByteString
byteBoundary B { bit0 = i0, bitsBody = bs } = case i0 of
	0 -> Right bs
	_ -> Left (B i0 (8 - i0) (BS.take 1 bs), BS.tail bs)

bitsToNum :: (Num n, Bits n) => [Bit] -> n
bitsToNum = foldr (\b s -> (case b of O -> 0; I -> 1) .|. s `shiftL` 1) 0

fromByteString :: BS.ByteString -> BitArray.B
fromByteString bs = BitArray.B {
	BitArray.bit0 = 0, BitArray.bitsLen = 8 * BS.length bs, BitArray.bitsBody = bs }

toByteString :: BitArray.B -> Either BitArray.B BS.ByteString
toByteString
	ba@BitArray.B { BitArray.bit0 = i0, BitArray.bitsLen = ln, BitArray.bitsBody = bs } = case (i0, ln `mod` 8) of
	(0, 0) -> Right bs; _ -> Left ba
