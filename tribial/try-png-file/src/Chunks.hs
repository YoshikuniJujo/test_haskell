{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Control.Monad
import "mtl" Control.Monad.Except
import Data.Typeable
import Data.ByteString qualified as BS
import System.IO.Unsafe

import Chunks.SomeChunk
import Chunks.Core qualified as C
import ReadPng
import Crc

chunk :: (BS.ByteString -> BS.ByteString -> SomeChunk) -> ReadPng SomeChunk
chunk f = do
	ln <- dataLength
	nm <- getChunkName
	dt <- pop ln
	c <- pop 4
	when (not . check (nm `BS.append` dt) $ bsToNum32 c) $ throwError "bad CRC"
	pure $ f nm dt

instance Chunk C.Ihdr where
	chunkName _ = "IHDR"
	chunkFromByteString _ bs = unsafePerformIO $ BS.useAsCStringLen bs \(pbs, pln) -> do
		p <- malloc
		copyBytes (castPtr p) pbs (min pln $ sizeOf (undefined :: C.Ihdr))
		C.Ihdr_ <$> newForeignPtr p (free p)
	chunkToByteString (C.Ihdr_ fp) = unsafePerformIO $ withForeignPtr fp \p ->
		BS.packCStringLen (castPtr p, sizeOf (undefined :: C.Ihdr))

data OtherChunk = OtherChunk {
	otherChunkName :: BS.ByteString,
	otherChunkData :: BS.ByteString } deriving (Show, Typeable)

instance Chunk OtherChunk where
	chunkName OtherChunk { otherChunkName = nm } = nm
	chunkFromByteString nm dt = OtherChunk nm dt
	chunkToByteString OtherChunk { otherChunkData = dt } = dt

chunkOther :: ReadPng SomeChunk
chunkOther = chunk (\case
	"IHDR" -> toChunk . chunkFromByteString @C.Ihdr "IHDR"
	nm -> toChunk . chunkFromByteString @OtherChunk nm)
