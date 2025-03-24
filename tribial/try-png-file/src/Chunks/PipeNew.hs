{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.PipeNew where

import Control.Monad
import Control.MonadClasses.State
import Control.MonadClasses.Except
import Data.Kind
import Data.Bits
import Data.Pipe
import Data.Maybe
import Data.ByteString qualified as BS

import Chunks.SomeChunk
import Chunks.MagicAndEnd
import Crc

takeByteString :: (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString a m), Monad m ) =>
	Int -> p BS.ByteString a m BS.ByteString
takeByteString n = do
	obs <- get
	let	ln = BS.length obs
	if ln >= n
	then BS.take n obs <$ modify (BS.drop n)
	else await >>= \case
		Just nbs -> if (n - ln > BS.length nbs)
			then do	put ("" :: BS.ByteString)
				bs' <- takeByteString (n - ln - BS.length nbs)
				pure $ obs `BS.append` nbs `BS.append` bs'
			else obs `BS.append` BS.take (n - ln) nbs
				<$ put (BS.drop (n - ln) nbs)
		Nothing -> pure obs

chunks :: forall (cs :: [Type]) -> (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString SomeChunk m),
	MonadError String (p BS.ByteString SomeChunk m),
	Monad m, DecodeChunks' (p BS.ByteString SomeChunk m) cs ) =>
	p BS.ByteString SomeChunk m ()
chunks cs = checkMagic >> chunk cs

checkMagic :: (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString a m),
	MonadError String (p BS.ByteString a m),
	Monad m ) =>
	p BS.ByteString a m ()
checkMagic = do
	mg <- takeByteString 8
	when (mg /= magic) $ throwError ("bad magic" :: String)

chunk :: forall (cs :: [Type]) -> (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString SomeChunk m),
	MonadError String (p BS.ByteString SomeChunk m),
	Monad m, DecodeChunks' (p BS.ByteString SomeChunk m) cs ) =>
	p BS.ByteString SomeChunk m ()
chunk cs = do
	mln <- dataLength
	case mln of
		Just ln -> do
			nm <- takeByteString 4
			bs <- takeByteString ln
			c <- takeByteString 4
			when (not . check (nm `BS.append` bs)
				. fromJust $ bsToNum32 c) $
				throwError @String "bad CRC"
			yield =<< decodeChunks' @_ @cs nm bs
			chunk cs
		Nothing -> pure ()

dataLength :: (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString a m), Monad m ) =>
	p BS.ByteString a m (Maybe Int)
dataLength = bsToNum32 <$> takeByteString 4

bsToNum32 :: (Bits n, Integral n) => BS.ByteString -> Maybe n
bsToNum32 bs
	| BS.length bs == 4 = Just . bigEndian 0 . (fromIntegral <$>) $ BS.unpack bs
	| otherwise = Nothing

bigEndian :: Bits n => n -> [n] -> n
bigEndian s [] = s
bigEndian s (n : ns) = bigEndian (s `shiftL` 8 .|. n) ns
