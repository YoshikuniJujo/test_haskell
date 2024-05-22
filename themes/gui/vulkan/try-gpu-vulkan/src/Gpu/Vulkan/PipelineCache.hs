{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache (

	-- * CREATE

	create, P, M.CreateInfo(..),

	-- * GET DATA

	getData, M.Data(..),

	-- * READ AND WRITE DATA

	readFile, writeFile, hRead, hWrite

	) where

import Prelude hiding (readFile, writeFile)

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry

import Gpu.Vulkan.PipelineCache.Type

import Gpu.Vulkan.AllocationCallbacks qualified as AllocationCallbacks
import Gpu.Vulkan.AllocationCallbacks.Type qualified as AllocationCallbacks
import Gpu.Vulkan.Device qualified as Device
import Gpu.Vulkan.Device.Type qualified as Device
import Gpu.Vulkan.PipelineCache.Middle qualified as M (
	create, destroy, CreateInfo(..), getData, Data(..) )

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Data.Word
import System.IO (Handle, hGetBuf, hPutBuf, withBinaryFile, IOMode(..))

import Data.ByteString qualified as BS

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mscc) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> (forall s . P s -> IO a) -> IO a
create (Device.D dv) ci
	(AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create dv ci mac) (\c -> M.destroy dv c mac) (f . P)

getData :: Device.D sd -> P s -> IO M.Data
getData (Device.D dv) (P c) = M.getData dv c

readFile :: FilePath -> IO M.Data
readFile fp = withBinaryFile fp ReadMode hRead

hRead :: Handle -> IO M.Data
hRead h = do
	sz <- readDataSize h
	allocaBytes (fromIntegral sz) \pd -> do
		_ <- hGetBuf h pd (fromIntegral sz)
		dataFromRaw sz pd

writeFile :: FilePath -> M.Data -> IO ()
writeFile fp d = withBinaryFile fp WriteMode (`hWrite` d)

hWrite :: Handle -> M.Data -> IO ()
hWrite h d = dataToRaw d \sz pd ->
	writeDataSize h sz >> hPutBuf h pd (fromIntegral sz)

readDataSize :: Handle -> IO Word64
readDataSize = readStorable

readStorable :: forall a . Storable a => Handle -> IO a
readStorable h = alloca \px -> hGetBuf h px (sizeOf @a undefined) >> peek px

writeDataSize :: Handle -> Word64 -> IO ()
writeDataSize = writeStorable

writeStorable :: Storable a => Handle -> a -> IO ()
writeStorable h x = alloca \px -> poke px x >> hPutBuf h px (sizeOf x)

dataFromRaw :: Word64 -> Ptr CChar -> IO M.Data
dataFromRaw sz pd = M.Data <$> BS.packCStringLen (pd, fromIntegral sz)

dataToRaw :: M.Data -> (Word64 -> Ptr CChar -> IO a) -> IO a
dataToRaw (M.Data bs) f = BS.useAsCStringLen bs \(pd, sz) -> f (fromIntegral sz) pd
