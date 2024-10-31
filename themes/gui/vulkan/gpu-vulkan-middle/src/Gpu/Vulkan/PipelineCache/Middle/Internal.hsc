{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache.Middle.Internal (
	P(..), CreateInfo(..), create, destroy,
	Data(..), getData,
	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Types
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Default
import Data.Word
import Data.ByteString qualified as BS

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.PipelineCache.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.PipelineCache.Core as C

#include <vulkan/vulkan.h>

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoInitialData :: Data }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

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
	withPoked ci f

newtype P = P C.P deriving Show

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO P
create (Device.D dvc) ci mac = P <$> alloca \pc -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac -> do
			r <- C.create dvc pci pac pc
			throwUnlessSuccess $ Result r
	peek pc

destroy :: Device.D -> P -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (P c) mac =
	AllocationCallbacks.mToCore mac $ C.destroy dvc c

getData :: Device.D -> P -> IO Data
getData (Device.D dv) (P c) = alloca \psz -> do
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

newtype Data = Data BS.ByteString deriving (Show, Eq)

data DataRaw = DataRaw #{type size_t} (Ptr CChar) deriving Show

instance Default Data where def = Data ""

instance Default DataRaw where def = DataRaw 0 nullPtr
