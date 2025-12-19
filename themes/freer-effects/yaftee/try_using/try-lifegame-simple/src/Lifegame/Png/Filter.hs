{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Png.Filter (filter) where

import Prelude hiding (filter)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Png qualified as Png
import Lifegame.Tools

filter :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Png.Header -> Int -> Int -> Eff.E es BSF.ByteString [Word8] ()
filter hdr w h = filterRaw hdr w h =<< Pipe.await

filterRaw :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Png.Header -> Int -> Int ->
	BSF.ByteString -> Eff.E es BSF.ByteString [Word8] ()
filterRaw hdr w h r0 = void do
	Pipe.yield $ Png.filter bypp zeros r0
	filterTail bypp (BSF.unpack r0) (h - 1)
	where
	zeros = ((w * Png.bpp hdr) `div'` 8) `replicate` 0
	bypp = Png.bpp hdr `div'` 8

filterTail :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Int -> [Word8] -> Int -> Eff.E es BSF.ByteString [Word8] ()
filterTail bypp pr = \case
	0 -> pure ()
	n -> Pipe.awaitMaybe >>= \case
		Nothing -> pure (); Just BSF.Empty -> pure ()
		Just bs -> do
			Pipe.yield $ Png.filter bypp pr bs
			filterTail bypp (BSF.unpack bs) (n - 1)
