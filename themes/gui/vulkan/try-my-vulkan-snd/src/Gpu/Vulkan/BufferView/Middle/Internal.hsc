{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView.Middle.Internal (
	B(..), CreateInfo(..), CreateFlags, create, destroy ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Monad.Cont
import Data.Word
import Data.Bits

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.BufferView.Core as C
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkBufferViewCreateFlags}
	[''Show, ''Storable, ''Eq, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoBuffer :: Buffer.B,
	createInfoFormat :: Format,
	createInfoOffset :: Device.Size,
	createInfoRange :: Device.Size }
	deriving Show

createInfoToCore :: Pokable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoBuffer = Buffer.B bf,
	createInfoFormat = Format fmt,
	createInfoOffset = Device.Size os,
	createInfoRange = Device.Size rng
	} = do
	(castPtr -> pnxt) <- ContT $ withPokedMaybe mnxt
	let	C.CreateInfo_ fci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoBuffer = bf,
			C.createInfoFormat = fmt,
			C.createInfoOffset = os,
			C.createInfoRange = rng }
	ContT $ withForeignPtr fci

newtype B = B C.B deriving Show

create :: (Pokable n, Pokable c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO B
create (Device.D dvc) ci mac = (B <$>) . ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	pci <- createInfoToCore ci
	pb <- ContT alloca
	lift do	r <- C.create dvc pci pac pb
		throwUnlessSuccess $ Result r
		peek pb

destroy :: Pokable d =>
	Device.D -> B -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (B b) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy dvc b pac
