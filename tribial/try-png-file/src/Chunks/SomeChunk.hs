{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.SomeChunk where

import "monads-tf" Control.Monad.Except
import Data.Kind
import Data.Typeable
import Data.ByteString qualified as BS

import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

data SomeChunk = forall c . Chunk c => SomeChunk c deriving Typeable

class Chunk c => CodecChunk c where
	chunkName :: BS.ByteString
	decodeChunk :: forall m . (MonadError m, ErrorType m ~ String) =>
		BS.ByteString -> m c
	encodeChunk :: c -> BS.ByteString

class Chunk c => CodecChunk' c where
	type CodecChunkArg c
	chunkName' :: BS.ByteString
	decodeChunk' :: forall m . (MC.MonadError String m) =>
		CodecChunkArg c -> BS.ByteString -> m c
	encodeChunk' :: c -> BS.ByteString

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
	decodeChunks :: (MonadError m, ErrorType m ~ String) =>
		BS.ByteString -> BS.ByteString -> m SomeChunk

instance DecodeChunks '[] where decodeChunks nm = pure . SomeChunk . OtherChunk nm

instance (CodecChunk c, DecodeChunks cs) => DecodeChunks (c ': cs) where
	decodeChunks :: forall m . (MonadError m, ErrorType m ~ String) =>
		BS.ByteString -> BS.ByteString -> m SomeChunk
	decodeChunks nm dt
		| nm == chunkName @c = SomeChunk <$> decodeChunk @c @m dt
		| otherwise = decodeChunks @cs nm dt

class DecodeChunks' m (cs :: [Type]) where
	decodeChunks' :: MC.MonadError String m =>
		BS.ByteString -> BS.ByteString -> m SomeChunk

instance DecodeChunks' m '[] where decodeChunks' nm = pure . SomeChunk . OtherChunk nm

instance (CodecChunk' c, MC.MonadState (CodecChunkArg c) m, DecodeChunks' m cs) => DecodeChunks' m (c ': cs) where
-- instance (CodecChunk' c, DecodeChunks' m cs) => DecodeChunks' m (c ': cs) where
	decodeChunks' :: MC.MonadError String m =>
		BS.ByteString -> BS.ByteString -> m SomeChunk
	decodeChunks' nm dt
		| nm == chunkName' @c = do
			a <- MC.get
			SomeChunk <$> decodeChunk' @c @m a dt
		| otherwise = decodeChunks' @m @cs nm dt
