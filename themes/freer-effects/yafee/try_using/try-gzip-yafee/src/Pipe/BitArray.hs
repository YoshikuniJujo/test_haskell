{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.BitArray where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.OpenUnion qualified as Union
import Data.Word
import Data.ByteString qualified as BS

import Pipe.ByteString.OnDemand
import BitArray (Bit(..))
import BitArray qualified as BitArray

bits' :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E (Pipe.P (Either BitArray.B BS.ByteString) BitArray.Bit ': effs) ()
bits' = popBit >>= \case
	Nothing -> pure ()
	Just b -> Pipe.yield b >> bits'

popBit :: (
	Union.Member (State.S Request) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (Except.E String) effs
	) =>
	Eff.E (Pipe.P (Either BitArray.B BS.ByteString) o ': effs) (Maybe BitArray.Bit)
popBit = State.getsN "bits" BitArray.pop >>= \case
	Nothing -> do
		State.put $ RequestBuffer 100
		State.putN "bits"
			. either id BitArray.fromByteString =<< getJust =<< Pipe.await
		popBit
	Just (b, ba') -> Just b <$ State.putN "bits" ba'

getJust :: Union.Member (Except.E String) effs => Maybe a -> Eff.E effs a
getJust = \case
	Nothing -> Except.throw @String "Not Just"
	Just x -> pure x

bitsToByteString :: (
	Union.Member (State.S BitQueue) effs
	) =>
	Eff.E (Pipe.P [Bit] BS.ByteString ': effs) ()
bitsToByteString = fix \go -> Pipe.await >>= \case
	Nothing -> pure ()
	Just bs -> do
		State.modify (`append` bs)
		Pipe.yield =<< putByteString
		go

putByteString :: (Union.Member (State.S BitQueue) effs) =>
	Eff.E effs BS.ByteString
putByteString = do
	q <- State.get
	let	(byts, bits) = unfoldr' popByte q
	BS.pack byts <$ State.put bits

type BitQueue = ([Bit], [Bit])

snoc :: BitQueue -> Bit -> BitQueue
snoc (xs, ys) b = (xs, b : ys)

append :: BitQueue -> [Bit] -> BitQueue
append (xs, ys) bs = (xs, reverse bs ++ ys)

uncons :: BitQueue -> Maybe (Bit, BitQueue)
uncons ([], []) = Nothing
uncons ([], ys) = uncons (reverse ys, [])
uncons (x : xs, ys) = Just (x, (xs, ys))

uncons' :: (
	Union.Member (State.S BitQueue) effs,
	Union.Member (Except.E String) effs
	) =>
	Eff.E effs Bit
uncons' = State.get >>= \bq -> case uncons bq of
	Nothing -> Except.throw "uncons: no bits"
	Just (b, bq') -> b <$ State.put bq'

popByte :: BitQueue -> Maybe (Word8, BitQueue)
popByte bq = either (\(_ :: String) -> Nothing) Just
	. Eff.run . Except.run . (`State.run` bq)
	$ BitArray.bitsToNum <$> replicateM 8 uncons'

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f s = case f s of
	Nothing -> ([], s)
	Just (x, s') -> (x :) `first` unfoldr' f s'
