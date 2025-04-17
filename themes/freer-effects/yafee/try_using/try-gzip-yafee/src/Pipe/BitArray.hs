{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.BitArray where

import Control.Monad
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

-- bitsToByteString =

-- putByteString = 

type BitQueue = ([Bit], [Bit])

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

popByte :: (
	Union.Member (State.S BitQueue) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E effs (Maybe Word8)
popByte = Just . BitArray.bitsToNum <$> replicateM 8 uncons'
