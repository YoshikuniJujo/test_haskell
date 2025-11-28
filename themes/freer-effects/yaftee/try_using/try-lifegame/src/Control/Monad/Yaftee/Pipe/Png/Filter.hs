{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Filter (pngFilter, pngUnfilter') where

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
import Data.Png.Filters

import Tools

pngUnfilter' :: (
	U.Member Pipe.P es,
	U.Member (Except.E String) es ) =>
	Header.Header -> Eff.E es BSF.ByteString [Word8] ()
pngUnfilter' hdr = void do
	bs <- Pipe.await
	let	bpp = Header.headerToBpp hdr
		rbs = Header.headerToRowBytes hdr
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

pngFilter :: (
	U.Member Pipe.P es,
	U.Member (Except.E String) es ) =>
	Header.Header ->
	BSF.ByteString -> [(Int, Int)] -> Eff.E es BSF.ByteString [Word8] ()
pngFilter _ _ [] = pure ()
pngFilter hdr bs0 ((w, h) : ss) = void do
	let	bpp = Header.headerToBpp hdr
		bd = fromIntegral $ Header.headerBitDepth hdr
		bs0' = filter bpp (replicate ((w * bd) `div'` 8 * Header.sampleNum' hdr) 0) bs0
	Pipe.yield bs0'
	filterAll bpp (BSF.unpack bs0) (h - 1)
	when (not $ null ss) do
		bs0'' <- Pipe.await
		pngFilter hdr bs0'' ss

filterAll :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Int -> [Word8] -> Int -> Eff.E es BSF.ByteString [Word8] ()
filterAll _bpp _prior 0 = pure ()
filterAll bpp prior n = Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just BSF.Empty -> pure ()
	Just bs -> do
		let	bs' = filter bpp prior bs
		Pipe.yield bs'
		filterAll bpp (BSF.unpack bs) (n - 1)
