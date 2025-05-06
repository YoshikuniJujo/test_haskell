{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.BitArray (

	-- * BIT ARRAY

	B, null, length, empty,

	-- * FROM/TO BYTE STRING

	fromByteString, toByteString,

	-- * APPEND

	append, appendByteString,

	-- * SPLIT

	pop, popBits, splitAt, byteBoundary,

	-- * CONVERT

	toBits, toBits'

	) where

import Prelude hiding (null, length, splitAt)
import Control.Arrow
import Data.Bits
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Bit qualified as Bit

data B = B { unB :: [B1] } deriving Show

empty :: B
empty = B []

null :: B -> Bool
null = \case (B []) -> True; _ -> False

length :: B -> Int
length  = sum . (length1 <$>) . unB

data B1 = B1 { zero :: Int, length1 :: Int, body :: BS.ByteString }
	deriving Show

empty1 :: B1
empty1 = B1 { zero = 0, length1 = 0, body = "" }

null1 :: B1 -> Bool
null1 = \case B1 { length1 = 0 } -> True; _ -> False

fromByteString :: BS.ByteString -> B
fromByteString bs = B [B1 { zero = 0, length1 = 8 * BS.length bs, body = bs }]

toByteString :: B -> Either B BS.ByteString
toByteString b@(B b1s) = case b1s of
	[] -> Right ""
	[b1] -> either (Left . B . (: [])) Right $ toByteString1 b1
	_ -> Left b

toByteString1 :: B1 -> Either B1 BS.ByteString
toByteString1 ba@B1 { zero = z, length1 = ln, body = bs } =
	case (z, ln `mod` 8) of (0, 0) -> Right bs; _ -> Left ba

append :: B -> B -> B
B b1 `append` B b2 = B . normalize $ b1 ++ b2

appendByteString :: B -> BS.ByteString -> B
appendByteString b = append b . fromByteString

normalize :: [B1] -> [B1]
normalize = go empty1 . (normalize1 <$>)
	where
	go B1 { length1 = 0 } = \case
		[] -> []
		b1' : b1s -> go b1' b1s
	go b1@B1 { zero = z1, length1 = ln1, body = bs1 } = \case
		[]	| ln1 > 0 -> [b1]
			| otherwise -> []
		B1 { length1 = 0 } : b1s -> go b1 b1s
		b1'@B1 { zero = z2, length1 = ln2, body = bs2 } : b1s
			| (z1 + ln1) `mod` 8 == 0, z2 == 0 ->
				go B1 {	zero = z1, length1 = ln1 + ln2,
					body = bs1 `BS.append` bs2 } b1s
			| (z1 + ln1 - z2) `mod` 8 == 0 ->
				go B1 { zero = z1, length1 = ln1 + ln2,
					body = BS.init bs1 `BS.append` (
						(	BS.last bs1 .&. bitMask 0 (z2 - 1) .|.
							BS.head bs2 .&. bitMask z2 7) `BS.cons`
						BS.tail bs2 ) } b1s
			| otherwise -> b1 : go b1' b1s

bitMask :: Bits n => Int -> Int -> n
bitMask i j = foldl setBit zeroBits [i .. j]

normalize1 :: B1 -> B1
normalize1 B1 { zero = z, length1 = ln, body = bs }
	| 0 <= z = B1 {
		zero = z',
		length1 = ln, body = BS.take t $ BS.drop (z `div` 8) bs }
	| otherwise = error "normalize: bad"
	where z' = z `mod` 8; t = (ln + z' - 1) `div` 8 + 1

pop :: B -> Maybe (Bit.B, B)
pop (B []) = Nothing
pop (B (b1 : b1s)) = case pop1 b1 of
	Nothing -> error "never occur"
	Just (b, b1') -> Just (b, B . normalize $ b1' : b1s )

pop1 :: B1 -> Maybe (Bit.B, B1)
pop1 B1 { zero = z, length1 = ln, body = bs } = case (z, ln) of
	(_, 0) -> Nothing
	(7, _) -> case BS.uncons bs of
		Just (b, bs') -> Just (
			bool Bit.O Bit.I (b `testBit` 7),
			B1 { zero = 0, length1 = ln - 1, body = bs' } )
		Nothing -> error "never occur"
	_ -> Just (
		bool Bit.O Bit.I $ BS.head bs `testBit` z,
		B1 { zero = z + 1, length1 = ln - 1, body = bs } )

splitAt :: Int -> B -> Maybe (B, B)
splitAt n0 = ((B *** B) <$>) . go n0 . unB
	where
	go 0 b1s = Just ([], b1s)
	go _ [] = Nothing
	go n (b1@B1 { length1 = ln } : b1s)
		| n < ln = case splitAt1 n b1 of
			Nothing -> error "never occur"
			Just (b1t, b1d) -> Just ([b1t], b1d : b1s)
		| n == ln = Just ([b1], b1s)
		| otherwise = ((b1 :) `first`) <$> go (n - ln) b1s

splitAt1 :: Int -> B1 -> Maybe (B1, B1)
splitAt1 n B1 { zero = z, length1 = ln, body = bs }
	| ln < n = Nothing
	| otherwise = Just (
		normalize1 B1 { zero = z, length1 = n, body = bs },
		normalize1 B1 { zero = z + n, length1 = ln - n, body = bs } )

byteBoundary :: B -> Either (B, B) B
byteBoundary (B b) = case go b of
	([], b2) -> Right $ B b2; b12 -> Left $ (B *** B) b12
	where
	go [] = ([], [])
	go (b1 : b1s) = case byteBoundary1 b1 of
		Nothing -> (b1 :) `first` go b1s
		Just (b1t, b1d)
			| null1 b1t -> ([], b1d : b1s)
			| otherwise -> ([b1t], b1d : b1s)

byteBoundary1 :: B1 -> Maybe (B1, B1)
byteBoundary1 b1@B1 { zero = z, length1 = ln, body = bs } = case z of
	0 -> Just (empty1, b1)
	_	| z + ln <= 8 -> Nothing
		| otherwise -> Just (
			B1 { zero = z, length1 = 8 - z, body = BS.take 1 bs },
			B1 { zero = 0, length1 = ln - 8 + z, body = BS.tail bs } )

popBits :: Bits n => Int -> B -> Maybe (n, B)
popBits ln b0
	| length b0 < ln = Nothing
	| otherwise = Just $ go 0 ln b0
	where
	go i j b = case (j, pop b) of
		(0, _) -> (zeroBits, b)
		(_, Nothing) -> error "never occur"
		(_, Just (bt, bts)) -> Bit.bit id (`setBit` i) bt `first` go (i + 1) (j - 1) bts

toBits :: Bits n => B -> n
toBits = go zeroBits
	where go i b = case pop b of
		Nothing -> zeroBits
		Just (bt, bts) -> Bit.bit id (`setBit` i) bt $ go (i + 1) bts

toBits' :: forall n . FiniteBits n => B -> Maybe n
toBits' b0
	| length b0 <= finiteBitSize (undefined :: n) = Just $ go 0 b0
	| otherwise = Nothing
	where go i b = case pop b of
		Nothing -> zeroBits
		Just (bt, bts) -> Bit.bit id (`setBit` i) bt $ go (i + 1) bts
