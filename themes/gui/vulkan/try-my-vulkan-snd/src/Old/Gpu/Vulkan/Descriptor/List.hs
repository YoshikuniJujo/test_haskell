{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Gpu.Vulkan.Descriptor.List where

import Foreign.Storable
import Data.HeteroList

import qualified Foreign.Storable.Generic

import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Buffer.Middle.Internal as Buffer.M
import qualified Gpu.Vulkan.Descriptor.Middle as M
import qualified Gpu.Vulkan.Descriptor.Core as C

import qualified Old.Gpu.Vulkan.Buffer.List.Type as Buffer.List
import qualified Old.Gpu.Vulkan.Buffer.List.Middle as Buffer.List.M

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

class BufferInfoListToMiddle slsmvs where
	bufferInfoListToMiddle ::
		HeteroVarList BufferInfo slsmvs -> [M.BufferInfo]

instance BufferInfoListToMiddle '[] where bufferInfoListToMiddle HVNil = []

instance (Foreign.Storable.Generic.G v, BufferInfoListToMiddle slsmvs) =>
	BufferInfoListToMiddle ('(sl, sm, v) ': slsmvs) where
	bufferInfoListToMiddle (bi :...: bis) =
		bufferInfoToMiddle bi : bufferInfoListToMiddle bis

bufferInfoListToCore :: BufferInfoListToMiddle slsmvs =>
	HeteroVarList BufferInfo slsmvs -> [C.BufferInfo]
bufferInfoListToCore = (M.bufferInfoToCore <$>) . bufferInfoListToMiddle
