{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bit (

	-- * BIT

	Bit(..), bit,

	-- * LIST

	bsToNum, bitsFromNum,

	-- * Queue

	Queue, append, popByte, uncons

	) where

import Control.Monad

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.OpenUnion qualified as Union
import Data.Bits (Bits, shiftL, testBit, (.|.))
import Data.Bool
import Data.Word

data Bit = O | I deriving (Show, Eq, Ord)

bit :: a -> a -> Bit -> a
bit x y = \case O -> x; I -> y

bsToNum :: (Num n, Bits n) => [Bit] -> n
bsToNum = foldr (\b s -> bit 0 1 b .|. s `shiftL` 1) 0

bitsFromNum :: Bits n => Int -> n -> [Bit]
bitsFromNum ln n = bool O I . (n `testBit`) <$> [0 .. ln - 1]

type Queue = ([Bit], [Bit])

append :: Queue -> [Bit] -> Queue
append (xs, ys) bs = (xs, reverse bs ++ ys)

uncons :: Queue -> Maybe (Bit, Queue)
uncons = \case
	([], []) -> Nothing
	([], ys) -> uncons (reverse ys, [])
	(x : xs, ys) -> Just (x, (xs, ys))

popByte :: Queue -> Maybe (Word8, Queue)
popByte bq = either (\(_ :: String) -> Nothing) Just . Eff.run
	. Except.run . (`State.run` bq) $ bsToNum <$> replicateM 8 uncons'

uncons' :: (
	Union.Member (State.S Queue) effs,
	Union.Member (Except.E String) effs ) => Eff.E effs i o Bit
uncons' = State.gets uncons >>=
	maybe (Except.throw "uncons: no bits") \(b, bq) -> b <$ State.put bq
