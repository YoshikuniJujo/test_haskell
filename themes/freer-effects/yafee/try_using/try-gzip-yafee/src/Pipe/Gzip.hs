{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Gzip (readHeader) where

import Control.Arrow
import Control.Monad
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Numeric

import Pipe.ByteString
import Pipe.IO
import Gzip
import BitArray
import ByteStringNum

readHeader :: (
	Union.Member (State.S BitArray) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (Pipe.P BS.ByteString ()) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,
	Union.Member IO effs ) =>
	Eff.E effs GzipHeaderRaw
readHeader = do
	mids <- takeBytes' @() 2
	case mids of
		Just ids | ids == "\US\139" -> putStrLn' "good magic"
		_ -> Except.throw ("bad magic" :: String)
	Just cm <- popByte' @()
	fs <- maybe (Except.throw @String "bad flags") pure . (readFlags =<<) =<< popByte' @()
	Just mt <- takeWord32' @()
	Just efs <- popByte' @()
	Just os <- popByte' @()
	bs' <- if (flagsRawExtra fs)
	then do	Just xln <- (fromIntegral <$>) <$> takeWord16' @()
		Just bs <- takeBytes' @() xln
		pure bs
	else pure ""
	fn <- if (flagsRawName fs) then takeString' else pure Nothing
	mcmmt <- if (flagsRawComment fs) then takeString' else pure Nothing
	mcrc16 <- if (flagsRawHcrc fs) then takeBytes @() 2 else pure Nothing
	maybeCheckHcrc mcrc16
	pure GzipHeaderRaw {
		gzipHeaderRawCompressionMethod = CompressionMethod cm,
		gzipHeaderRawFlags = fs,
		gzipHeaderRawModificationTime = word32ToCTime mt,
		gzipHeaderRawExtraFlags = efs,
		gzipHeaderRawOperatingSystem = OS os,
		gzipHeaderRawExtraField = decodeExtraFields bs',
		gzipHeaderRawFileName = fn,
		gzipHeaderRawComment = mcmmt }

takeString' :: (
	Union.Member (State.S Crc) effs,
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString ()) effs
	) =>
	Eff.E effs (Maybe BS.ByteString)
takeString' = State.gets (spanUntil 0 . bitsBody) >>= \case
	Nothing -> do
		b <- readMore @()
		if b then takeString' else pure Nothing
	Just (t, d) -> Just t <$ (
		State.put (BitArray 0 (BS.length d * 8) d) >> calcCrc' (t `BS.snoc` 0)
		)

takeWord32' :: forall o effs . (
	Union.Member (State.S Crc) effs,
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) => Eff.E effs (Maybe Word32)
takeWord32' = (bsToNum <$>) <$> takeBytes' @o 4

takeWord16' :: forall o effs . (
	Union.Member (State.S Crc) effs,
	Union.Member (State.S BitArray) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) => Eff.E effs (Maybe Word16)
takeWord16' = (bsToNum <$>) <$> takeBytes' @o 2

maybeCheckHcrc :: (
	Union.Member (State.S Crc) effs,
	Union.Member (Except.E String) effs ) =>
	Maybe BS.ByteString -> Eff.E effs ()
maybeCheckHcrc = \case
	Nothing -> pure ()
	Just crc16' -> do
		Crc crc32 <- State.get
		when (bsToNum @Word16 crc16' /= fromIntegral (complement crc32)) . Except.throw
			$ "bad: " ++ showHex @Word16 (bsToNum crc16') "" ++ " " ++ showHex @Word32 crc32 ""

spanUntil :: Word8 -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
spanUntil b0 bs = case BS.uncons bs of
	Nothing -> Nothing
	Just (b, bs')
		| b == b0 -> Just ("", bs')
		| otherwise -> ((b `BS.cons`) `first`) <$> spanUntil b0 bs'
