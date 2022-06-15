{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Atom where

import Foreign.Storable

import qualified Foreign.Storable.Generic

import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.Atom as Buffer.Atom
import qualified Vulkan.Descriptor.Core as C

import qualified Vulkan.Descriptor.Middle as M

data BufferInfo v = BufferInfo {
	bufferInfoBuffer :: Buffer.Atom.B v,
	bufferInfoOffset :: Device.Size }
	deriving Show

bufferInfoToCore :: forall v . Storable (Foreign.Storable.Generic.Wrap v) =>
	BufferInfo v -> C.BufferInfo
bufferInfoToCore BufferInfo {
	bufferInfoBuffer = Buffer.Atom.B b,
	bufferInfoOffset = Device.Size sz } = C.BufferInfo {
	C.bufferInfoBuffer = b,
	C.bufferInfoOffset = sz,
	C.bufferInfoRange = fromIntegral
		$ sizeOf @(Foreign.Storable.Generic.Wrap v) undefined }
