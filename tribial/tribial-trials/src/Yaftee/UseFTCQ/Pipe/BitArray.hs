{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.BitArray where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.OpenUnion qualified as Union
import Data.Word
import Data.ByteString qualified as BS

import Yaftee.UseFTCQ.Pipe.ByteString.OnDemand
import Data.BitArray (Bit(..))
import Data.BitArray qualified as BitArray

import Debug.Trace

bits' :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E effs (Either BitArray.B BS.ByteString) BitArray.Bit ()
bits' = popBit >>= \case
	Nothing -> pure ()
	Just b -> Pipe.yield b >> bits'

popBit :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (Except.E String) effs
	) =>
	Eff.E effs (Either BitArray.B BS.ByteString) o (Maybe BitArray.Bit)
popBit = State.getsN "bits" BitArray.pop >>= \case
	Nothing -> do
		trace "popBit: Nothing" $ pure ()
		State.put $ RequestBuffer 100
		trace "popBit: after put RequestBuffer" $ pure ()
		State.putN "bits"
			. either id BitArray.fromByteString =<< Pipe.await
		popBit
	Just (b, ba') -> Just b <$ State.putN "bits" ba'

getJust :: Union.Member (Except.E String) effs => Maybe a -> Eff.E effs i o a
getJust = \case
	Nothing -> Except.throw @String "Not Just"
	Just x -> pure x

bitsToByteString :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S BitArray.BitQueue) effs ) =>
	Eff.E effs [Bit] BS.ByteString ()
bitsToByteString = fix \go -> Pipe.await >>= \case
	bs -> do
		State.modify (`BitArray.appendQ` bs)
		Pipe.yield =<< putByteString
		go

putByteString :: (Union.Member (State.S BitArray.BitQueue) effs) =>
	Eff.E effs i o BS.ByteString
putByteString = do
	q <- State.get
	let	(byts, bits) = unfoldr' popByte q
	BS.pack byts <$ State.put bits

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f s = case f s of
	Nothing -> ([], s)
	Just (x, s') -> (x :) `first` unfoldr' f s'

popByte :: BitArray.BitQueue -> Maybe (Word8, BitArray.BitQueue)
popByte bq = either (\(_ :: String) -> Nothing) Just
	. Eff.run . Except.run . (`State.run` bq)
	$ BitArray.bitsToNum <$> replicateM 8 uncons'

uncons' :: (
	Union.Member (State.S BitArray.BitQueue) effs,
	Union.Member (Except.E String) effs
	) =>
	Eff.E effs i o Bit
uncons' = State.get >>= \bq -> case BitArray.uncons bq of
	Nothing -> Except.throw "uncons: no bits"
	Just (b, bq') -> b <$ State.put bq'
