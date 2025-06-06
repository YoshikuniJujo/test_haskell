{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Sequence.BitArray (

	-- * BIT ARRAY

	B, null, length, empty,

	-- * FROM/TO SEQUENCE

	fromSequence, toSequence,

	-- * APPEND

	append, appendSequence,

	-- * SPLIT

	pop, splitAt, byteBoundary, popBits,

	-- * CONVERT

	toBits, toBits'

	) where

import Prelude hiding (null, length, head, tail, init, last, splitAt)
import Prelude qualified as P
import Control.Arrow
import Data.Bits
import Data.Sequence qualified as Seq
import Data.Bool
import Data.Word

-- * DATA

data B = B { unB :: [B1] } deriving Show

empty :: B; empty = B []
null :: B -> Bool; null = \case (B []) -> True; _ -> False
length :: B -> Int; length = sum . (length1 <$>) . unB

data B1 = B1 { zero :: Int, length1 :: Int, body :: Seq.Seq Word8 }
	deriving Show

empty1 :: B1; empty1 = B1 { zero = 0, length1 = 0, body = Seq.empty }
null1 :: B1 -> Bool; null1 = \case B1 { length1 = 0 } -> True; _ -> False

-- * FROM/TO SEQUENCE

fromSequence :: Seq.Seq Word8 -> B
fromSequence Seq.Empty = B []
fromSequence s = B [B1 { zero = 0, length1 = 8 * P.length s, body = s }]

toSequence :: B -> Either B (Seq.Seq Word8)
toSequence b@(B b1s) = case b1s of
	[] -> Right Seq.Empty
	[b1] -> either (Left . B . (: [])) Right $ toSequence1 b1
	_ -> Left b

toSequence1 :: B1 -> Either B1 (Seq.Seq Word8)
toSequence1 b@B1 { zero = z, length1 = ln, body = s } =
	case (z, ln `mod` 8) of (0, 0) -> Right s; _ -> Left b

-- * APPEND

append :: B -> B -> B
B b1 `append` B b2 = B . normalize $ b1 ++ b2

appendSequence :: B -> Seq.Seq Word8 -> B
appendSequence b = append b . fromSequence

normalize :: [B1] -> [B1]
normalize = go empty1 . (normalize1 <$>)
	where
	go B1 { length1 = 0 } = \case
		[] -> []
		b1' : b1s -> go b1' b1s
	go b1@B1 { zero = z1, length1 = ln1, body = s1 } = \case
		[]	| ln1 > 0 -> [b1]
			| otherwise -> []
		B1 { length1 = 0 } : b1s -> go b1 b1s
		b1'@B1 { zero = z2, length1 = ln2, body = s2 } : b1s
			| (z1 + ln1) `mod` 8 == 0, z2 == 0 ->
				go B1 {	zero = z1, length1 = ln1 + ln2,
					body = s1 Seq.>< s2 } b1s
			| (z1 + ln1 - z2) `mod` 8 == 0 ->
				go B1 {	zero = z1, length1 = ln1 + ln2,
					body = init s1 Seq.>< (
						(	last s1 .&. bitMask 0 (z2 - 1) .|.
							head s2 .&. bitMask z2 7) Seq.:<|
						tail s2 ) } b1s
			| otherwise -> b1 : go b1' b1s

bitMask :: Bits n => Int -> Int -> n
bitMask i j = foldl setBit zeroBits [i .. j]

head :: Seq.Seq a -> a
head (x Seq.:<| _) = x
head Seq.Empty = error "head: empty"

tail :: Seq.Seq a -> Seq.Seq a
tail (_ Seq.:<| s) = s
tail Seq.Empty = error "tail: empty"

init :: Seq.Seq a -> Seq.Seq a
init (s Seq.:|> _) = s
init Seq.Empty = error "init: empty"

last :: Seq.Seq a -> a
last (_ Seq.:|> x) = x
last Seq.Empty = error "last: empty"

normalize1 :: B1 -> B1
normalize1 B1 { zero = z, length1 = ln, body = s }
	| 0 <= z = B1 {
		zero = z',
		length1 = ln, body = Seq.take t $ Seq.drop (z `div` 8) s }
	| otherwise = error "normalize: bad"
	where z' = z `mod` 8; t = (ln + z' - 1) `div` 8 + 1

-- * SPLIT

pop :: B -> Maybe (Bool, B)
pop (B []) = Nothing
pop (B (b1 : b1s)) = case pop1 b1 of
	Nothing -> error $ "never occur: " ++ show b1 ++ " " ++ show b1s
	Just (b, b1') -> Just (b, B . normalize $ b1' : b1s)

pop1 :: B1 -> Maybe (Bool, B1)
pop1 B1 { zero = z, length1 = ln, body = s } = case (z, ln) of
	(_, 0) -> Nothing
	(7, _) -> case uncons s of
		Just (b, s') -> Just (
			b `testBit` 7,
			B1 { zero = 0, length1 = ln - 1, body = s' } )
		Nothing -> error "never occur"
	_ -> Just (
		head s `testBit` z,
		B1 { zero = z + 1, length1 = ln - 1, body = s } )

uncons :: Seq.Seq a -> Maybe (a, Seq.Seq a)
uncons = \case Seq.Empty -> Nothing; x Seq.:<| s -> Just (x, s)

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
splitAt1 n B1 { zero = z, length1 = ln, body = s }
	| ln < n = Nothing
	| otherwise = Just (
		normalize1 B1 { zero = z, length1 = n, body = s },
		normalize1 B1 { zero = z + n, length1 = ln - n, body = s } )

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
byteBoundary1 b1@B1 { zero = z, length1 = ln, body = s } = case z of
	0 -> Just (empty1, b1)
	_	| z + ln <= 8 -> Nothing
		| otherwise -> Just (
			B1 { zero = z, length1 = 8 - z, body = Seq.take 1 s },
			B1 { zero = 0, length1 = ln - 8 + z, body = tail s } )

popBits :: Bits n => Int -> B -> Maybe (n, B)
popBits ln b0
	| length b0 < ln = Nothing
	| otherwise = Just $ go 0 ln b0
	where
	go i j b = case (j, pop b) of
		(0, _) -> (zeroBits, b)
		(_, Nothing) -> error "never occur"
		(_, Just (bt, bts)) ->
			bool id (`setBit` i) bt `first` go (i + 1) (j - 1) bts

-- * TO BITS

toBits :: Bits n => B -> n
toBits = go zeroBits
	where go i b = case pop b of
		Nothing -> zeroBits
		Just (bt, bts) -> bool id (`setBit` i) bt $ go (i + 1) bts

toBits' :: forall n . FiniteBits n => B -> Maybe n
toBits' b0
	| length b0 <= finiteBitSize (undefined :: n) = Just $ go 0 b0
	| otherwise = Nothing
	where go i b = case pop b of
		Nothing -> zeroBits
		Just (bt, bts) -> bool id (`setBit` i) bt $ go (i + 1) bts
