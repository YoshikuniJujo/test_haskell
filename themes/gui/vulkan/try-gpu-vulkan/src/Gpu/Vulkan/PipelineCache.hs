{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache (

	-- * CREATE

	create, P,

	-- * GET DATA

	getData,

	-- * READ AND WRITE DATA

	readData, writeData

	) where

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
	create, destroy, CreateInfo, getData, Data(..) )

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Data.Word
import System.IO

import Data.ByteString qualified as BS

create :: (WithPoked (TMaybe.M mn), AllocationCallbacks.ToMiddle mscc) =>
	Device.D sd -> M.CreateInfo mn ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> (forall s . P s -> IO a) -> IO a
create (Device.D dv) ci
	(AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create dv ci mac) (\c -> M.destroy dv c mac) (f . P)

getData :: Device.D sd -> P s -> IO M.Data
getData (Device.D dv) (P c) = M.getData dv c

readData :: FilePath -> IO M.Data
readData fp =
	withBinaryFile fp ReadMode \h -> do
	sz <- readDataSize h
	allocaBytes (fromIntegral sz) \pd -> do
		_ <- hGetBuf h pd (fromIntegral sz)
		dataFromRaw sz pd

writeData :: FilePath -> M.Data -> IO ()
writeData fp d = dataToRaw d \sz pd -> withBinaryFile fp WriteMode \h ->
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
