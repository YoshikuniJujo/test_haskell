{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.Pipe where

import Data.Kind
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Except
import Data.Bits
import Data.Pipe
import Data.Maybe
import Data.ByteString qualified as BS

import Chunks.SomeChunk
import Crc

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

chunks :: forall (cs :: [Type]) -> (
	MonadState m, StateType m ~ BS.ByteString,
	MonadError m, ErrorType m ~ String, DecodeChunks cs ) =>
	Pipe BS.ByteString SomeChunk m ()
chunks cs = checkMagic >> chunk' cs

magic :: BS.ByteString
magic = "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a"

checkMagic :: (
	MonadState m, StateType m ~ BS.ByteString,
	MonadError m, ErrorType m ~ String ) =>
	Pipe BS.ByteString SomeChunk m ()
checkMagic = do
	mg <- takeByteString 8
	when (mg /= magic) $ throwError "bad magic"

chunk' :: forall (cs :: [Type]) -> (
	MonadState m, StateType m ~ BS.ByteString,
	MonadError m, ErrorType m ~ String, DecodeChunks cs ) =>
	Pipe BS.ByteString SomeChunk m ()
chunk' cs = do
	mln <- dataLength
	case mln of
		Just ln -> do
			nm <- takeByteString 4
			bs <- takeByteString ln
			c <- takeByteString 4
			when (not . check (nm `BS.append` bs) . fromJust $ bsToNum32 c) $ throwError "bad CRC"
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

data End = End deriving Show

instance CodecChunk End where
	chunkName = "IEND"
	decodeChunk = \case "" -> End; _ -> error "bad end"

instance Chunk End
