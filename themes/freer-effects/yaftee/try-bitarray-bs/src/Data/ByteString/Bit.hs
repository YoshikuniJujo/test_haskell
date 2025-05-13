{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.Bit (

	-- * BIT

	B(..), bit,

	-- * LIST

	listToNum, listFromNum,

	-- * QUEUE

	Queue, empty, append, uncons, popByte

	) where

import Control.Arrow
import Data.Bits hiding (bit)
import Data.Bool
import Data.Word

data B = O | I deriving (Show, Eq, Ord)

bit :: a -> a -> B -> a
bit x y = \case O -> x; I -> y

listToNum :: Bits n => [B] -> n
listToNum = foldr (\b s -> bit id (`setBit` 0) b $ s `shiftL` 1) zeroBits

listFromNum :: Bits n => Int -> n -> [B]
listFromNum ln n = bool O I . (n `testBit`) <$> [0 .. ln - 1]

type Queue = ([B], [B])

empty :: Queue
empty = ([], [])

append :: Queue -> [B] -> Queue
append (xs, ys) bs = (xs, reverse bs ++ ys)

uncons :: Queue -> Maybe (B, Queue)
uncons = \case
	([], []) -> Nothing
	([], ys) -> uncons (reverse ys, [])
	(x : xs, ys) -> Just (x, (xs, ys))

popByte :: Queue -> Maybe (Word8, Queue)
popByte = ((listToNum `first`) <$>) . unfoldrN 8 uncons

unfoldrN :: Int -> (a -> Maybe (b, a)) -> a -> Maybe ([b], a)
unfoldrN 0 _ x = Just ([], x)
unfoldrN n f x = case f x of
	Nothing -> Nothing
	Just (y, x') -> ((y :) `first`) <$> unfoldrN (n - 1) f x'
