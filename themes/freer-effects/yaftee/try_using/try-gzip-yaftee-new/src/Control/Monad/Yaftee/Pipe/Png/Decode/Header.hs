{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode.Header where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.Png.Header

read :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Header) es,
	U.Member (Except.E String) es ) =>
	(Header -> Eff.E es (Either x LBS.ByteString) o ()) ->
		Eff.E es (Either x LBS.ByteString) o ()
read nm proc = do
	State.putN nm $ OnDemand.RequestBytes 4
	w <- LBS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	h <- LBS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	bd <- LBS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	ct <- LBS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- LBS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	fm <- LBS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	im <- LBS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
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
