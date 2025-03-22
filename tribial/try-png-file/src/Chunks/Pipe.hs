{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.Pipe where

import Data.Kind
import "monads-tf" Control.Monad.State
import Data.Pipe
import Data.Bits
import Data.ByteString qualified as BS

import Chunks.SomeChunk

takeByteString :: (MonadState m, StateType m ~ BS.ByteString) =>
	Int -> Pipe BS.ByteString a m BS.ByteString
takeByteString n = do
	obs <- get
	let	ln = BS.length obs
	if ln >= n
	then BS.take n obs <$ modify (BS.drop n)
	else do	mbs <- await
		case mbs of
			Just nbs -> do
				if (n - ln > BS.length nbs)
				then do put ""
					bs' <- takeByteString (n - ln - BS.length nbs)
					pure $ obs `BS.append` nbs `BS.append` bs'
				else obs `BS.append` BS.take (n - ln) nbs
					<$ put (BS.drop (n - ln) nbs)
			Nothing -> pure obs

chunk :: (MonadState m, StateType m ~ BS.ByteString) =>
	(BS.ByteString -> BS.ByteString -> SomeChunk) ->
	Pipe BS.ByteString SomeChunk m ()
chunk f = do
	mln <- dataLength
	case mln of
		Just ln -> do
			nm <- takeByteString 4
			bs <- takeByteString ln
			yield $ f nm bs
			chunk f
		Nothing -> pure ()

chunk' :: forall (cs :: [Type]) -> (DecodeChunks cs, MonadState m, StateType m ~ BS.ByteString) =>
	Pipe BS.ByteString SomeChunk m ()
chunk' cs = do
	mln <- dataLength
	case mln of
		Just ln -> do
			nm <- takeByteString 4
			bs <- takeByteString ln
			_ <- takeByteString 4
			yield $ decodeChunks @cs nm bs
			chunk' cs
		Nothing -> pure ()

dataLength :: (MonadState m, StateType m ~ BS.ByteString) =>
	Pipe BS.ByteString a m (Maybe Int)
dataLength = bsToNum32 <$> takeByteString 4

bsToNum32 :: (Bits n, Integral n) => BS.ByteString -> Maybe n
bsToNum32 bs
	| BS.length bs == 4 = Just . bigEndian 0 . (fromIntegral <$>) $ BS.unpack bs
	| otherwise = Nothing

bigEndian :: Bits n => n -> [n] -> n
bigEndian s [] = s
bigEndian s (n : ns) = bigEndian (s `shiftL` 8 .|. n) ns
