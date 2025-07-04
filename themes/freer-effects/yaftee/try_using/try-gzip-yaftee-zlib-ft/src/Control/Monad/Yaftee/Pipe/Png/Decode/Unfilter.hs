{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter (pngUnfilter) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Word
import Data.ByteString.FingerTree qualified as BSF

import Data.Png.Header qualified as Header
import Data.Png.Filters

pngUnfilter :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (Except.E String) es ) =>
	Eff.E es BSF.ByteString [Word8] ()
pngUnfilter nm = void do
	bs <- Pipe.await
	h <- State.getN nm
	let	bpp = Header.headerToBpp h
		rbs = Header.headerToRowBytes h
	bs' <- either Except.throw pure
		$ unfilter bpp (replicate rbs 0) bs
	Pipe.yield bs'
	unfilterAll bpp bs'

unfilterAll :: (
	U.Member Pipe.P es,
	U.Member (Except.E String) es ) =>
	Int -> [Word8] -> Eff.E es BSF.ByteString [Word8] ()
unfilterAll bpp prior = Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just BSF.Empty -> pure ()
	Just bs -> do
		bs' <- either Except.throw pure $ unfilter bpp prior bs
		Pipe.yield bs'
		unfilterAll bpp bs'
