{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode.Header where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.Png.Header

read :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (Except.E String) es ) =>
	(Header -> Eff.E es (Either x BS.ByteString) o ()) ->
		Eff.E es (Either x BS.ByteString) o ()
read nm proc = do
	State.putN nm $ OnDemand.RequestBytes 4
	w <- BS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	h <- BS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	bd <- BS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	ct <- BS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	cm <- BS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	fm <- BS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	State.putN nm $ OnDemand.RequestBytes 1
	im <- BS.toBitsBE <$> (Except.getRight msgNotRight =<< Pipe.await)
	proc Header {
		headerWidth = w, headerHeight = h,
		headerBitDepth = bd,
		headerColorType = ct,
		headerCompressionMethod = cm,
		headerFilterMethod = fm,
		headerInterlaceMethod = im }

msgNotRight :: String
msgNotRight = "not right"
