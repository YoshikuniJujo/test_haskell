{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Image.Middle where

import Foreign.Ptr
import Foreign.Pointable
import Data.IORef

import Gpu.Vulkan.Memory.Tmp

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
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
	Maybe (AllocationCallbacks.A n') -> IO (Device.Size, Device.MemoryImage)
allocate dvc img ai mac = do
	mai <- allocateInfoToMiddle dvc img ai
	(\(Device.Memory mem) -> do
		m <- readIORef mem
		pure (M.allocateInfoAllocationSize mai, Device.MemoryImage m))
		=<< M.allocate dvc mai mac

free :: Pointable n =>
	Device.D -> Device.MemoryImage -> Maybe (AllocationCallbacks.A n) ->
	IO ()
free dvc (Device.MemoryImage m) mac = do
	mem <- newIORef m
	M.free dvc (Device.Memory mem) mac

map :: Device.D -> Device.MemoryImage -> Device.Size -> M.MapFlags -> IO (Ptr a)
map dvc (Device.MemoryImage m) sz flgs = do
	mem <- newIORef m
	M.map dvc (Device.Memory mem) 0 sz flgs

unmap :: Device.D -> Device.MemoryImage -> IO ()
unmap dvc (Device.MemoryImage m) = do
	mem <- newIORef m
	M.unmap dvc $ Device.Memory mem
