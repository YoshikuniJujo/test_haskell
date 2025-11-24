{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode.Header where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Png.Header
import Data.Png.Header.Data

import Data.Word.Word8 qualified as BSF
import Data.ByteString.FingerTree qualified as BSF

read :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Header) es,
	U.Member (Except.E String) es ) =>
	(Header -> Eff.E es BSF.ByteString o ()) ->
		Eff.E es BSF.ByteString o ()
read nm proc = do
	State.putN nm $ OnDemand.RequestBytes 4
	w <- BSF.toBitsBE <$> Pipe.await
	h <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	bd <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	ct <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	fm <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	im <- BSF.toBitsBE <$> Pipe.await
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
