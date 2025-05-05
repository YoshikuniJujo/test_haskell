{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.BitArray where

import Data.Bits
import Data.ByteString qualified as BS

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
			| (z1 + ln1 + z2) `mod` 8 == 0 ->
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
