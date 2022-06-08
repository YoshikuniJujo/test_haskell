{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.Image (
	allocate, M.AllocateInfo(..), readByteString) where

import Foreign.Pointable
import Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Type as Device
import qualified Vulkan.Device.Middle as Device.M
import qualified Vulkan.Memory.Middle as Memory.M
import qualified Vulkan.Image.Type as Image
import qualified Vulkan.Memory.Image.Middle as M

allocate :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> Image.I si -> M.AllocateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . Device.MemoryImage s -> IO a) -> IO a
allocate (Device.D dvc) (Image.I img) ai macc macd f = bracket
	(M.allocate dvc img ai macc) (\mi -> M.free dvc mi macd)
	(f . Device.MemoryImage)

readByteString :: Device.D sd ->
	Device.MemoryImage si -> Memory.M.MapFlags -> IO BS.ByteString
readByteString (Device.D dvc)
	(Device.MemoryImage m@(Device.M.MemoryImage (fromIntegral -> sz) _))
	flgs = bracket (M.map dvc m flgs) (const $ M.unmap dvc m) \src ->
	BS.create sz \dst -> BS.memcpy dst src sz
