{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.SomeChunk where

import Data.Kind
import Data.Typeable
import Data.ByteString qualified as BS

data SomeChunk = forall c . Chunk c => SomeChunk c deriving Typeable

class Chunk c => CodecChunk c where
	chunkName :: BS.ByteString
	decodeChunk :: BS.ByteString -> c
	encodeChunk :: c -> BS.ByteString

class (Typeable c, Show c) => Chunk c where

	toChunk :: c -> SomeChunk
	toChunk = SomeChunk

	fromChunk :: SomeChunk -> Maybe c
	fromChunk (SomeChunk c) = cast c

instance Show SomeChunk where
	show (SomeChunk c) = show c

instance Chunk SomeChunk where toChunk = id; fromChunk = Just

fromSomeChunk :: SomeChunk -> (forall c . Chunk c => c -> a) -> a
fromSomeChunk (SomeChunk c) f = f c

data OtherChunk = OtherChunk {
	otherChunkName :: BS.ByteString,
	otherChunkData :: BS.ByteString } deriving (Show, Typeable)

instance Chunk OtherChunk where

class DecodeChunks (cs :: [Type]) where
	decodeChunks :: BS.ByteString -> BS.ByteString -> SomeChunk

instance DecodeChunks '[] where decodeChunks nm = SomeChunk . OtherChunk nm

instance (CodecChunk c, DecodeChunks cs) => DecodeChunks (c ': cs) where
	decodeChunks nm dt
		| nm == chunkName @c = SomeChunk $ decodeChunk @c dt
		| otherwise = decodeChunks @cs nm dt
