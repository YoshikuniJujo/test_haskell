{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.ChunkEncode.Old (chunks, Chunk(..)) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.HigherOpenUnion qualified as U
import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF

import Control.Monad.Yaftee.Pipe.Png.ChunkEncode

chunks :: U.Member Pipe.P es => a -> Eff.E es (a -> (Chunk, a)) BSF.ByteString a
chunks x = Pipe.yield fileHeader >> fvr x \st -> Pipe.await >>= \f -> do
	let	(c, st') = f st
	st' <$ Pipe.yield (chunkToByteString c)
	where fvr st a = a st >>= (`fvr` a)

chunkToByteString :: Chunk -> BSF.ByteString
chunkToByteString Chunk { chunkName = nm, chunkBody = bd } =
	BSF.fromBitsBE' ln <> nmbd <> BSF.fromBitsBE' (Crc32.toWord crc)
	where
	ln = fromIntegral @_ @Word32 $ BSF.length bd
	nmbd = nm <> bd
	crc = Crc32.complement $ BSF.foldl' Crc32.step Crc32.initial nmbd

fileHeader :: BSF.ByteString
fileHeader = "\x89PNG\r\n\SUB\n"
