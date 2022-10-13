{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Gpu.Vulkan.Descriptor.Atom where

import Foreign.Storable

import qualified Foreign.Storable.Generic

import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer.M
import qualified Gpu.Vulkan.Descriptor.Core as C

import qualified Gpu.Vulkan.Descriptor.Middle.Internal as M

import qualified Old.Gpu.Vulkan.Buffer.Atom as Buffer.Atom

data BufferInfo v = BufferInfo {
	bufferInfoBuffer :: Buffer.Atom.B v }
	deriving Show

bufferInfoToCore :: forall v . Storable (Foreign.Storable.Generic.Wrap v) =>
	BufferInfo v -> C.BufferInfo
bufferInfoToCore = M.bufferInfoToCore . bufferInfoToMiddle

bufferInfoToMiddle :: forall v . Storable (Foreign.Storable.Generic.Wrap v) =>
	BufferInfo v -> M.BufferInfo
bufferInfoToMiddle BufferInfo {
	bufferInfoBuffer = Buffer.Atom.B b } = M.BufferInfo {
		M.bufferInfoBuffer = Buffer.M.B b,
		M.bufferInfoOffset = 0,
		M.bufferInfoRange = Device.Size . fromIntegral
			$ sizeOf @(Foreign.Storable.Generic.Wrap v) undefined }
