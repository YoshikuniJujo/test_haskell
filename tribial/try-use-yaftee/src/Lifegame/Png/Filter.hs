{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Png.Filter (filter, Size) where

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
	Png.Header -> [Size] -> Eff.E es BSF.ByteString [Word8] ()
filter hdr ss = flip (filterRaw hdr) ss =<< Pipe.await

type Size = (Int, Int)

filterRaw :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Png.Header ->
	BSF.ByteString -> [Size] -> Eff.E es BSF.ByteString [Word8] ()
filterRaw hdr r0 = \case
	[] -> pure ()
	(w, h) : ss -> void do
		Pipe.yield $ Png.filter bypp (zeros w) r0
		filterTail bypp (BSF.unpack r0) (h - 1)
		case ss of
			[] -> pure ()
			_ -> flip (filterRaw hdr) ss =<< Pipe.await
	where
	zeros w = ((w * Png.bpp hdr) `div'` 8) `replicate` 0
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
