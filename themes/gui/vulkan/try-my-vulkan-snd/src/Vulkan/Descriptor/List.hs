{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.List where

import Foreign.Storable

import qualified Foreign.Storable.Generic

import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.Middle as Buffer.M
import qualified Vulkan.Buffer.List.Type as Buffer.List
import qualified Vulkan.Buffer.List.Middle as Buffer.List.M
import qualified Vulkan.Descriptor.Middle as M
import qualified Vulkan.Descriptor.Core as C

data BufferInfo slsmv where
	BufferInfo :: { bufferInfoBuffer :: Buffer.List.Binded sl sm v } ->
		BufferInfo '(sl, sm, v)

deriving instance Show (BufferInfo slsmv)

bufferInfoToCore :: forall v sl sm . Foreign.Storable.Generic.G v =>
	BufferInfo '(sl, sm, v) -> C.BufferInfo
bufferInfoToCore = M.bufferInfoToCore . bufferInfoToMiddle

bufferInfoToMiddle :: forall v sl sm . Foreign.Storable.Generic.G v =>
	BufferInfo '(sl, sm, v) -> M.BufferInfo
bufferInfoToMiddle BufferInfo {
	bufferInfoBuffer = Buffer.List.Binded (Buffer.List.M.B ln b) } = M.BufferInfo {
		M.bufferInfoBuffer = Buffer.M.B b,
		M.bufferInfoOffset = 0,
		M.bufferInfoRange = Device.Size . fromIntegral . (ln *)
			$ sizeOf @(Foreign.Storable.Generic.Wrap v) undefined }
