{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode.Header where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.BitArray.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Sequence qualified as Seq
import Data.Sequence.Word8 qualified as Seq
import Data.Word
import Data.Png.Header

read :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Header) es,
	U.Member (Except.E String) es ) =>
	(Header -> Eff.E es (Either x (Seq.Seq Word8)) o ()) ->
		Eff.E es (Either x (Seq.Seq Word8)) o ()
read nm proc = do
	State.putN nm $ OnDemand.RequestBytes 4
	w <- Seq.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	h <- Seq.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	bd <- Seq.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	ct <- Seq.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- Seq.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	fm <- Seq.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	im <- Seq.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	let	hdr = Header {
			headerWidth = w, headerHeight = h,
			headerBitDepth = bd,
			headerColorType = ct,
			headerCompressionMethod = cm,
			headerFilterMethod = fm,
			headerInterlaceMethod = im }
	proc hdr
	State.putN nm hdr

msgNotRight :: String
msgNotRight = "not right"
