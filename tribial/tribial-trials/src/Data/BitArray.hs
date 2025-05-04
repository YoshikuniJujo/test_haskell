{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.BitArray (

	-- * BIT ARRAY

	B, null, empty,

	-- * FROM/TO BYTE STRING

	fromByteString, toByteString,

	-- * APPEND

	append, appendByteString,

	-- * SPLIT

	pop, splitAt, byteBoundary',

	-- * CONVERT

	toWord8

	) where

import Prelude hiding (null, length, take, splitAt)

import Data.Bits
import Data.Bit (pattern O, pattern I)
import Data.Bit qualified as Bit
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

-- BIT ARRAY

data B = B { zero :: Int, length :: Int, body :: BS.ByteString } deriving Show

null :: B -> Bool
null = \case B { length = 0 } -> True; _ -> False

empty :: B
empty = B { zero = 0, length = 0, body = "" }

-- FROM/TO BYTESTRing

fromByteString :: BS.ByteString -> B
fromByteString bs = B { zero = 0, length = 8 * BS.length bs, body = bs }

toByteString :: B -> Either B BS.ByteString
toByteString ba@B { zero = z, length = ln, body = bs } = case (z, ln `mod` 8) of
	(0, 0) -> Right bs; _ -> Left ba

-- APPEND

append :: B -> B -> B
append	B { zero = z1, length = ln1, body = bs1 }
	B { zero = z2, length = ln2, body = bs2 }
	| (z1 + ln1 + z2) `mod` 8 == 0 =
		B { zero = z1, length = ln1 + ln2, body = bs1 `BS.append` bs2 }
	| otherwise = error "bad"

appendByteString :: B -> BS.ByteString -> B
appendByteString B { zero = z, length = ln, body = bs } bs'
	| (z + ln) `mod` 8 == 0 = B {
		zero = z,
		length = ln + 8 * BS.length bs', body = bs `BS.append` bs' }
	| otherwise = error "bad"

-- SPLIT

pop :: B -> Maybe (Bit.Bit, B)
pop B { zero = z, length = ln, body = bs } = case (z, ln) of
	(_, 0) -> Nothing
	(7, _) -> case BS.uncons bs of
		Just (b, bs') -> Just (
			bool O I (b `testBit` 7),
			B { zero = 0, length = ln - 1, body = bs' } )
		Nothing -> error "never occur foo"
	_ -> Just (	
		bool O I $ BS.head bs `testBit` z,
		B { zero = z + 1, length = ln - 1, body = bs } )

splitAt :: Int -> B -> Maybe (B, B)
splitAt n (B i ln bs)
	| ln < n = Nothing
	| otherwise =
		Just (normalize $ B i n bs, normalize $ B (i + n) (ln - n) bs)

normalize :: B -> B
normalize (B i ln bs)
	| 0 <= i = B {
		zero = i',
		length = ln, body = BS.take t $ BS.drop (i `div` 8) bs }
	| otherwise = error "normalize: bad"
	where i' = i `mod` 8; t = (ln + i' - 1) `div` 8 + 1

byteBoundary' :: B -> Either (B, B) B
byteBoundary' b@B { zero = z, length = ln, body = bs } = case z of
	0 -> Right b
	_ -> Left (
		B { zero = z, length = 8 - z, body = BS.take 1 bs },
		B { zero = 0, length = ln -8 + z, body = BS.tail bs } )

-- CONVERT

toWord8 :: B -> Maybe Word8
toWord8 (B i ln bs)
	| ln + i <= 8 =
		Just $ b0 `shiftR` i .&. foldl setBit zeroBits [0 .. ln - 1]
	| ln <= 8 = Just $
		b0 `shiftR` i .|.
		b1 `shiftL` (8 - i) .&. foldl setBit zeroBits [0 .. ln - 1]
	| otherwise = Nothing
	where b0 = BS.head bs; b1 = BS.head $ BS.tail bs
