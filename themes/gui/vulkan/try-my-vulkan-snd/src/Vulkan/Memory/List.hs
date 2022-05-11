{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.List where

import Foreign.Pointable
import Data.Word

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device as Device
import qualified Vulkan.Buffer.List as Buffer
import qualified Vulkan.Memory.Middle as M

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoMemoryTypeIndex :: Word32 }
	deriving Show

allocateInfoToMiddle :: Device.D -> Buffer.B v -> AllocateInfo n -> IO (M.AllocateInfo n)
allocateInfoToMiddle dvc b AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoMemoryTypeIndex = mti }= do
	memRequirement <- Buffer.getMemoryRequirements dvc b
	pure M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoAllocationSize =
			M.requirementsSize memRequirement,
		M.allocateInfoMemoryTypeIndex = mti }

allocate :: (Pointable n, Pointable n') =>
	Device.D -> Buffer.B v -> AllocateInfo n -> Maybe (AllocationCallbacks.A n') ->
	IO (Device.MemoryList v)
allocate dvc b ai mac = do
	mai <- allocateInfoToMiddle dvc b ai
	(\(Device.Memory m) -> Device.MemoryList m) <$> M.allocate dvc mai mac

free :: Pointable n =>
	Device.D -> Device.MemoryList v -> Maybe (AllocationCallbacks.A n) ->
	IO ()
free dvc (Device.MemoryList m) mac = M.free dvc (Device.Memory m) mac
