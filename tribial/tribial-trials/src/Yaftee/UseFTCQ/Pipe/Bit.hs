{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Bit (bsToByteString) where

import Control.Arrow
import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.OpenUnion qualified as Union
import Data.Bit qualified as Bit
import Data.ByteString qualified as BS

bsToByteString :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Bit.Queue) effs ) =>
	Eff.E effs [Bit.B] BS.ByteString ()
bsToByteString = fix \go -> Pipe.await >>= \case
	bs -> do
		State.modify (`Bit.append` bs)
		Pipe.yield =<< putByteString
		go

putByteString :: (Union.Member (State.S Bit.Queue) effs) =>
		Eff.E effs i o BS.ByteString
putByteString = do
	q <- State.get
	let	(byts, bits) = unfoldr' Bit.popByte q
	BS.pack byts <$ State.put bits

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f s = case f s of
	Nothing -> ([], s)
	Just (x, s') -> (x :) `first` unfoldr' f s'
