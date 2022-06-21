{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Image.Middle where

import Foreign.Ptr
import Foreign.Pointable

import Gpu.Vulkan.Memory

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Memory.Middle as M
import qualified Gpu.Vulkan.Image.Middle as Image

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoMemoryTypeIndex :: TypeIndex }
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
	(\(Device.Memory m) ->
			Device.MemoryImage (M.allocateInfoAllocationSize mai) m)
		<$> M.allocate dvc mai mac

free :: Pointable n =>
	Device.D -> Device.MemoryImage -> Maybe (AllocationCallbacks.A n) ->
	IO ()
free dvc (Device.MemoryImage _ m) mac = M.free dvc (Device.Memory m) mac

map :: Device.D -> Device.MemoryImage -> M.MapFlags -> IO (Ptr a)
map dvc (Device.MemoryImage sz m) flgs = M.map dvc (Device.Memory m) 0 sz flgs

unmap :: Device.D -> Device.MemoryImage -> IO ()
unmap dvc (Device.MemoryImage _ m) = M.unmap dvc $ Device.Memory m
