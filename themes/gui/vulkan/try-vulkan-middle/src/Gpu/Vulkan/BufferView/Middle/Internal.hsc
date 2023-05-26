{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.BufferView.Middle.Internal (
	B(..), CreateInfo(..), CreateFlags, create, destroy ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
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

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoBuffer :: Buffer.B,
	createInfoFormat :: Format,
	createInfoOffset :: Device.Size,
	createInfoRange :: Device.Size }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore' :: WithPoked (TMaybe.M mn) => CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore' CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoBuffer = Buffer.B bf,
	createInfoFormat = Format fmt,
	createInfoOffset = Device.Size os,
	createInfoRange = Device.Size rng
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	withPoked C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoBuffer = bf,
			C.createInfoFormat = fmt,
			C.createInfoOffset = os,
			C.createInfoRange = rng } f

newtype B = B C.B deriving Show

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO B
create (Device.D dvc) ci mac = B <$> alloca \pb -> do
	createInfoToCore' ci \pci -> AllocationCallbacks.mToCore mac \pac -> do
		r <- C.create dvc pci pac pb
		throwUnlessSuccess $ Result r
	peek pb

destroy :: Device.D -> B -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (B b) mac =
	AllocationCallbacks.mToCore mac $ C.destroy dvc b
