{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.SomeChunk where

import Data.Typeable
import Data.ByteString qualified as BS

data SomeChunk = forall c . Chunk c => SomeChunk c deriving Typeable

class (Typeable c, Show c) => Chunk c where
	chunkName :: c -> BS.ByteString
	chunkFromByteString :: BS.ByteString -> BS.ByteString -> c
	chunkToByteString :: c -> BS.ByteString

	toChunk :: c -> SomeChunk
	toChunk = SomeChunk

	fromChunk :: SomeChunk -> Maybe c
	fromChunk (SomeChunk c) = cast c

instance Show SomeChunk where
	show (SomeChunk c) = show c

instance Chunk SomeChunk where
	chunkName (SomeChunk c) = chunkName c
--	chunkFromByteString 

fromSomeChunk :: SomeChunk -> (forall c . Chunk c => c -> a) -> a
fromSomeChunk (SomeChunk c) f = f c
