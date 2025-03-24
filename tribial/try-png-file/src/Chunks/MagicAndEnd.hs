{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Chunks.MagicAndEnd where

import Control.MonadClasses.Except
import Data.ByteString qualified as BS

import Chunks.SomeChunk

magic :: BS.ByteString
magic = "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a"

data Iend = Iend deriving Show

instance CodecChunk' Iend where
	type CodecChunkArg Iend = ()
	chunkName' = "IEND"
	decodeChunk' a = \case "" -> pure Iend; _ -> throwError @String "bad end"
	encodeChunk' Iend = ""

instance Chunk Iend
