{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.Atom where

import Foreign.Storable
import Foreign.Pointable
import Data.Word

import qualified Foreign.Storable.Generic

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device as Device
import qualified Vulkan.Buffer.Atom as Buffer
import qualified Vulkan.Memory.Middle as M

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoMemoryTypeIndex :: Word32 }
	deriving Show

allocateInfoToMiddle ::
	Device.D -> Buffer.B v -> AllocateInfo n -> IO (M.AllocateInfo n)
allocateInfoToMiddle dvc b AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoMemoryTypeIndex = mti } = do
	memRequirement <- Buffer.getMemoryRequirements dvc b
	pure M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoAllocationSize =
			M.requirementsSize memRequirement,
		M.allocateInfoMemoryTypeIndex = mti }

allocate :: (Pointable n, Pointable n') =>
	Device.D -> Buffer.B v -> AllocateInfo n ->
	Maybe (AllocationCallbacks.A n') -> IO (Device.MemoryAtom v)
allocate dvc b ai mac = do
	mai <- allocateInfoToMiddle dvc b ai
	(\(Device.Memory m) -> Device.MemoryAtom m) <$> M.allocate dvc mai mac

free :: Pointable n =>
	Device.D -> Device.MemoryAtom v -> Maybe (AllocationCallbacks.A n) ->
	IO ()
free dvc (Device.MemoryAtom m) = M.free dvc (Device.Memory m)

write :: Foreign.Storable.Generic.G v =>
	Device.D -> Device.MemoryAtom v -> M.MapFlags -> v -> IO ()
write dvc (Device.Memory . (\(Device.MemoryAtom m) -> m) -> mem) flgs v = do
	dat <- M.map dvc mem 0 (fromIntegral $ sizeOf v') flgs
	poke dat v'
	M.unmap dvc mem
	where v' = Foreign.Storable.Generic.Wrap v
