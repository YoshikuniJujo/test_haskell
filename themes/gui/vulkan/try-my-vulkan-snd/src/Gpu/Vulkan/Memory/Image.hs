{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Image (
	allocate, M.AllocateInfo(..), readByteString) where

import Foreign.Pointable
import Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Image.Type as Image
import qualified Gpu.Vulkan.Memory.Image.Middle as M

allocate :: (Pointable n, Pointable n2, Pointable n3) =>
	Device.D sd -> Image.I si -> M.AllocateInfo n ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . Device.MemoryImage s -> IO a) -> IO a
allocate (Device.D dvc) (Image.I img) ai macc macd f = bracket
	(M.allocate dvc img ai macc) (\(_, mi) -> M.free dvc mi macd)
	(\(sz, mi) -> f $ Device.MemoryImage sz mi)

readByteString :: Device.D sd ->
	Device.MemoryImage si -> Memory.M.MapFlags -> IO BS.ByteString
readByteString (Device.D dvc)
	(Device.MemoryImage sz_@(fromIntegral -> sz) m)
	flgs = bracket (M.map dvc m sz_ flgs) (const $ M.unmap dvc m) \src -> do
	print sz
	BS.create sz \dst -> BS.memcpy dst src sz
