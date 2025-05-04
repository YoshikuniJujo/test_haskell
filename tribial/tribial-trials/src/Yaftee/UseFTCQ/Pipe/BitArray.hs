{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.BitArray where

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.OpenUnion qualified as Union
import Data.ByteString qualified as BS

import Yaftee.UseFTCQ.Pipe.ByteString.OnDemand
import Data.Bit qualified as Bit
import Data.BitArrayNew qualified as BitArray

import Debug.Trace

bits :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.Named "bits" BitArray.B) effs,
	Union.Member (Except.E String) effs ) =>
	Eff.E effs (Either BitArray.B BS.ByteString) Bit.Bit ()
bits = popBit >>= \case
	Nothing -> pure ()
	Just b -> Pipe.yield b >> bits
	where
	popBit :: (
		Union.Member Pipe.P effs,
		Union.Member (State.S Request) effs,
		Union.Member (State.Named "bits" BitArray.B) effs,
		Union.Member (Except.E String) effs
		) =>
		Eff.E effs (Either BitArray.B BS.ByteString) o (Maybe Bit.Bit)
	popBit = State.getsN "bits" BitArray.pop >>= \case
		Nothing -> do
			trace "popBit: Nothing" $ pure ()
			State.put $ RequestBuffer 100
			trace "popBit: after put RequestBuffer" $ pure ()
			State.putN "bits"
				. either id BitArray.fromByteString =<< Pipe.await
			popBit
		Just (b, ba') -> Just b <$ State.putN "bits" ba'
