{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bit (

	-- * BIT

	B(..), bit,

	-- * LIST

	bsToNum, bsFromNum,

	-- * Queue

	Queue, append, uncons

	) where

import Data.Bits (Bits, shiftL, testBit, (.|.))
import Data.Bool

data B = O | I deriving (Show, Eq, Ord)

bit :: a -> a -> B -> a
bit x y = \case O -> x; I -> y

bsToNum :: (Num n, Bits n) => [B] -> n
bsToNum = foldr (\b s -> bit 0 1 b .|. s `shiftL` 1) 0

bsFromNum :: Bits n => Int -> n -> [B]
bsFromNum ln n = bool O I . (n `testBit`) <$> [0 .. ln - 1]

type Queue = ([B], [B])

append :: Queue -> [B] -> Queue
append (xs, ys) bs = (xs, reverse bs ++ ys)

uncons :: Queue -> Maybe (B, Queue)
uncons = \case
	([], []) -> Nothing
	([], ys) -> uncons (reverse ys, [])
	(x : xs, ys) -> Just (x, (xs, ys))
