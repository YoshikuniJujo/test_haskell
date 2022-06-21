{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Descriptor.Atom where

import Foreign.Storable

import qualified Foreign.Storable.Generic

import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Buffer.Atom as Buffer.Atom
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.Descriptor.Core as C

import qualified Gpu.Vulkan.Descriptor.Middle as M

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
