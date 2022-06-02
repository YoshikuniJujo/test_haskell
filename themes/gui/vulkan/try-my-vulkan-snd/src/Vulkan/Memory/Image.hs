{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.Image where

import Foreign.Pointable
import Data.Word

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Memory.Middle as M
import qualified Vulkan.Image.Middle as Image

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoMemoryTypeIndex :: Word32 }
	deriving Show

allocateInfoToMiddle ::
	Device.D -> Image.I -> AllocateInfo n -> IO (M.AllocateInfo n)
allocateInfoToMiddle dvc img AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoMemoryTypeIndex = mti } = do
	memRequirement <- Image.getMemoryRequirements dvc img
	pure M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoAllocationSize =
			M.requirementsSize memRequirement,
		M.allocateInfoMemoryTypeIndex = mti }

allocate :: (Pointable n, Pointable n') =>
	Device.D -> Image.I -> AllocateInfo n ->
	Maybe (AllocationCallbacks.A n') -> IO Device.MemoryImage
allocate dvc img ai mac = do
	mai <- allocateInfoToMiddle dvc img ai
	(\(Device.Memory m) -> Device.MemoryImage m) <$> M.allocate dvc mai mac

free :: Pointable n =>
	Device.D -> Device.MemoryImage -> Maybe (AllocationCallbacks.A n) ->
	IO ()
free dvc (Device.MemoryImage m) mac = M.free dvc (Device.Memory m) mac
