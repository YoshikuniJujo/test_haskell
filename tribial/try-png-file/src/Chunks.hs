{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks where

import Control.Monad
import "mtl" Control.Monad.Except
import Data.ByteString qualified as BS

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

chunkOther :: ReadPng SomeChunk
chunkOther = chunk \case
--	"IHDR" -> toChunk . decodeChunk @C.Ihdr
	nm -> toChunk . OtherChunk nm
