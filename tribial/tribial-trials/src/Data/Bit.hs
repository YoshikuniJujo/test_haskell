{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bit (

	-- * BIT

	B(..), bit,

	-- * LIST

	listToNum, listFromNum,

	-- * Queue

	Queue, append, uncons, popByte

	) where

import Control.Monad

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.OpenUnion qualified as Union
import Data.Bits (Bits, shiftL, testBit, (.|.))
import Data.Bool
import Data.Word

data B = O | I deriving (Show, Eq, Ord)

bit :: a -> a -> B -> a
bit x y = \case O -> x; I -> y

listToNum :: (Num n, Bits n) => [B] -> n
listToNum = foldr (\b s -> bit 0 1 b .|. s `shiftL` 1) 0

listFromNum :: Bits n => Int -> n -> [B]
listFromNum ln n = bool O I . (n `testBit`) <$> [0 .. ln - 1]

type Queue = ([B], [B])

append :: Queue -> [B] -> Queue
append (xs, ys) bs = (xs, reverse bs ++ ys)

uncons :: Queue -> Maybe (B, Queue)
uncons = \case
	([], []) -> Nothing
	([], ys) -> uncons (reverse ys, [])
	(x : xs, ys) -> Just (x, (xs, ys))

popByte :: Queue -> Maybe (Word8, Queue)
popByte bq = either (\(_ :: String) -> Nothing) Just . Eff.run
	. Except.run . (`State.run` bq) $ listToNum <$> replicateM 8 uncons'

uncons' :: (
	Union.Member (State.S Queue) effs,
	Union.Member (Except.E String) effs ) => Eff.E effs i o B
uncons' = State.gets uncons >>=
	maybe (Except.throw "uncons: no bits") \(b, bq) -> b <$ State.put bq
