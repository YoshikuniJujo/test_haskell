{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Png.Filter (filter, unfilter, Size) where

import Prelude hiding (filter)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header qualified as Header
import Data.Png.Header.Data qualified as Header
import Data.Png.Filter qualified as Filter

type Size = (Int, Int)

filter :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Header.H -> [Size] -> Eff.E es BSF.ByteString [Word8] ()
filter hdr ss = (\bs -> filterRaw hdr bs ss) =<< Pipe.await

filterRaw :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Header.H ->
	BSF.ByteString -> [Size] -> Eff.E es BSF.ByteString [Word8] ()
filterRaw _ _ [] = pure ()
filterRaw hdr r0 ((w, h) : ss) = void do
	Pipe.yield $ Filter.filter bpp (Header.rowBytes hdr w `replicate` 0) r0
	filterTail bpp (BSF.unpack r0) (h - 1)
	case ss of
		[] -> pure ()
		_ -> (\bs -> filterRaw hdr bs ss) =<< Pipe.await
	where bpp = Header.headerToBpp hdr

filterTail :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Int -> [Word8] -> Int -> Eff.E es BSF.ByteString [Word8] ()
filterTail _ _ 0 = pure ()
filterTail bpp pr n = Pipe.awaitMaybe >>= \case
	Nothing -> pure (); Just BSF.Empty -> pure ()
	Just bs -> do
		Pipe.yield $ Filter.filter bpp pr bs
		filterTail bpp (BSF.unpack bs) (n - 1)

unfilter :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Header.H -> Eff.E es BSF.ByteString [Word8] ()
unfilter hdr = void $ Pipe.await >>= \bs ->
	(>>) <$> Pipe.yield <*> unfilterTail bpp =<< either Except.throw pure
		(Filter.unfilter bpp (replicate rbs 0) bs)
	where bpp = Header.headerToBpp hdr; rbs = Header.headerToRowBytes hdr

unfilterTail :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Int -> [Word8] -> Eff.E es BSF.ByteString [Word8] ()
unfilterTail bpp prior = Pipe.awaitMaybe >>= \case
	Nothing -> pure (); Just BSF.Empty -> pure ()
	Just bs -> (>>) <$> Pipe.yield <*> unfilterTail bpp
		=<< either Except.throw pure (Filter.unfilter bpp prior bs)
