{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache.Middle.Internal (
	C(..), CreateInfo(..), Data(..), create, destroy, getData ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Data.Default
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.PipelineCache.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.PipelineCache.Core as C

#include <vulkan/vulkan.h>

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoInitialData :: Data }
	deriving Show

data Data = Data #{type size_t} (Ptr ()) deriving Show

instance Default Data where def = Data 0 nullPtr

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoInitialData = Data dtsz pdt } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoInitialDataSize = dtsz,
			C.createInfoPInitialData = pdt } in
	withPoked ci f

newtype C = C C.C deriving Show

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO C
create (Device.D dvc) ci mac = C <$> alloca \pc -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCore mac \pac -> do
			r <- C.create dvc pci pac pc
			throwUnlessSuccess $ Result r
	peek pc

destroy :: WithPoked d =>
	Device.D -> C -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (C c) mac =
	AllocationCallbacks.maybeToCore mac $ C.destroy dvc c

getData :: Device.D -> C -> IO Data
getData (Device.D dv) (C c) = alloca \psz -> do
	r <- C.getData dv c psz nullPtr
	throwUnlessSuccess $ Result r
	sz <- peek psz
	allocaBytes (fromIntegral sz) \pdt -> do
		r' <- C.getData dv c psz pdt
		throwUnlessSuccess $ Result r'
		pure $ Data sz pdt
