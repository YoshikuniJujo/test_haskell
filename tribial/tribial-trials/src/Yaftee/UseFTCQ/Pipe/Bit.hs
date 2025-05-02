{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Bit where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.OpenUnion qualified as Union
import Data.Word
import Data.Bit qualified as Bit
import Data.ByteString qualified as BS

bitsToByteString :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Bit.Queue) effs ) =>
	Eff.E effs [Bit.B] BS.ByteString ()
bitsToByteString = fix \go -> Pipe.await >>= \case
	bs -> do
		State.modify (`Bit.append` bs)
		Pipe.yield =<< putByteString
		go
	where
	putByteString :: (Union.Member (State.S Bit.Queue) effs) =>
		Eff.E effs i o BS.ByteString
	putByteString = do
		q <- State.get
		let	(byts, bits) = unfoldr' popByte q
		BS.pack byts <$ State.put bits
	unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
	unfoldr' f s = case f s of
		Nothing -> ([], s)
		Just (x, s') -> (x :) `first` unfoldr' f s'
	popByte :: Bit.Queue -> Maybe (Word8, Bit.Queue)
	popByte bq = either (\(_ :: String) -> Nothing) Just
		. Eff.run . Except.run . (`State.run` bq)
		$ Bit.bsToNum <$> replicateM 8 uncons'
	uncons' :: (
		Union.Member (State.S Bit.Queue) effs,
		Union.Member (Except.E String) effs
		) =>
		Eff.E effs i o Bit.B
	uncons' = State.get >>= \bq -> case Bit.uncons bq of
		Nothing -> Except.throw "uncons: no bits"
		Just (b, bq') -> b <$ State.put bq'
