{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandPool.Middle.Internal (
	C(..), CreateInfo(..), create, destroy ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke (WithPoked, withPokedMaybe', withPtrS)

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.CommandPool.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily
import qualified Gpu.Vulkan.CommandPool.Core as C

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueFamilyIndex :: QueueFamily.Index }
	deriving Show

createInfoToCore ::
	WithPoked n => CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueFamilyIndex = QueueFamily.Index qfi
	} f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoQueueFamilyIndex = qfi } in
	withForeignPtr fCreateInfo f

newtype C = C C.C deriving Show

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO C
create (Device.D dvc) ci mac = C <$> alloca \pc -> do
	createInfoToCore ci \pci -> AllocationCallbacks.maybeToCore mac \pac ->
		throwUnlessSuccess . Result =<< C.create dvc pci pac pc
	peek pc

destroy :: WithPoked d =>
	Device.D -> C -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (C c) mac =
	AllocationCallbacks.maybeToCore mac $ C.destroy dvc c
