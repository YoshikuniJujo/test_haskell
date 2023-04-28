{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache.Middle.Internal (
	C(..), CreateInfo(..), create, destroy,
	Data(..), getData, writeData, readData,

	tryCreateAndPrintData
	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Types
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Default
import Data.Word
import Data.ByteString qualified as BS
import System.IO

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.PipelineCache.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.PipelineCache.Core as C

import Data.Bits

#include <vulkan/vulkan.h>

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoInitialData :: Data }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

tryCreateAndPrintData :: Device.D -> C -> IO ()
tryCreateAndPrintData dv@(Device.D cdv) c@(C cc) = alloca \psz -> do
	r <- C.getData cdv cc psz nullPtr
	throwUnlessSuccess $ Result r
	sz <- peek psz
	allocaBytes (fromIntegral sz) \pdt -> do
		r' <- C.getData cdv cc psz pdt
		throwUnlessSuccess $ Result r'
		d <- dataFromRaw . DataRaw sz $ castPtr pdt
		let	ci = CreateInfo {
				createInfoNext = TMaybe.N,
				createInfoFlags = zeroBits,
				createInfoInitialData = d }
			cci = C.CreateInfo {
				C.createInfoPNext = nullPtr,
				C.createInfoFlags = zeroBits,
				C.createInfoInitialDataSize = sz,
				C.createInfoPInitialData = pdt }
		alloca \pc -> withPoked cci \pcci -> do
			C.create cdv pcci nullPtr pc
			c' <- peek pc
			print =<< getData dv (C c')
--		c' <- create @() @() dv ci Nothing
--		print =<< getData dv c'

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoInitialData = d } f =
	dataToRaw d \(DataRaw dtsz pdt) ->
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoInitialDataSize = dtsz,
			C.createInfoPInitialData = castPtr pdt } in
	print dtsz >>
	withPoked ci f

newtype C = C C.C deriving Show

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> Maybe (AllocationCallbacks.A c) -> IO C
create (Device.D dvc) ci mac = C <$> alloca \pc -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCoreNew mac \pac -> do
			r <- C.create dvc pci pac pc
			throwUnlessSuccess $ Result r
	peek pc

destroy :: Device.D -> C -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (C c) mac =
	AllocationCallbacks.maybeToCoreNew mac $ C.destroy dvc c

getData :: Device.D -> C -> IO Data
getData (Device.D dv) (C c) = alloca \psz -> do
	r <- C.getData dv c psz nullPtr
	throwUnlessSuccess $ Result r
	sz <- peek psz
	allocaBytes (fromIntegral sz) \pdt -> do
		r' <- C.getData dv c psz pdt
		throwUnlessSuccess $ Result r'
		dataFromRaw . DataRaw sz $ castPtr pdt

dataFromRaw :: DataRaw -> IO Data
dataFromRaw (DataRaw sz pd) = Data <$> BS.packCStringLen (pd, fromIntegral sz)

dataToRaw :: Data -> (DataRaw -> IO a) -> IO a
dataToRaw (Data bs) f = BS.useAsCStringLen bs \(pd, sz) ->
	f $ DataRaw (fromIntegral sz) pd

newtype Data = Data BS.ByteString deriving Show

data DataRaw = DataRaw #{type size_t} (Ptr CChar) deriving Show

instance Default Data where def = Data ""

instance Default DataRaw where def = DataRaw 0 nullPtr

writeData :: FilePath -> Data -> IO ()
writeData fp d = dataToRaw d (writeDataRaw fp)

readData :: FilePath -> IO Data
readData fp = -- alloca \pd -> dataFromRaw =<< readDataRaw fp pd
	withBinaryFile fp ReadMode \h -> do
	sz <- readDataSize h
	allocaBytes (fromIntegral sz) \pd -> do
		_ <- hGetBuf h pd (fromIntegral sz)
		dataFromRaw $ DataRaw sz pd

writeDataRaw :: FilePath -> DataRaw -> IO ()
writeDataRaw fp (DataRaw sz pd) = withBinaryFile fp WriteMode \h ->
	writeStorable h sz >> hPutBuf h pd (fromIntegral sz)

readDataSize :: Handle -> IO #{type size_t}
readDataSize h = readStorable h

readDataRaw :: FilePath -> Ptr CChar -> IO DataRaw
readDataRaw fp pd = withBinaryFile fp ReadMode \h -> do
	sz <- readStorable h
	_ <- hGetBuf h pd $ fromIntegral sz
	pure $ DataRaw sz pd

writeStorable :: Storable a => Handle -> a -> IO ()
writeStorable h x = alloca \px -> poke px x >> hPutBuf h px (sizeOf x)

readStorable :: forall a . Storable a => Handle -> IO a
readStorable h = alloca \px -> hGetBuf h px (sizeOf @a undefined) >> peek px
