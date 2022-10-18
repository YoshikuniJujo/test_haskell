{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Gpu.Vulkan.Memory.Atom where

import Foreign.Storable
import Foreign.Pointable
import Data.IORef

import qualified Foreign.Storable.Generic

import Gpu.Vulkan.Memory.Tmp

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.Memory.Middle as M

import qualified Old.Gpu.Vulkan.Buffer.Atom as Buffer

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoMemoryTypeIndex :: TypeIndex }
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
	(\(Device.Memory mem) -> do
		m <- readIORef mem
		pure $ Device.MemoryAtom m)
		=<< M.allocate dvc mai mac

free :: Pointable n =>
	Device.D -> Device.MemoryAtom v -> Maybe (AllocationCallbacks.A n) ->
	IO ()
free dvc (Device.MemoryAtom m) mac = do
	mem <- newIORef m
	M.free dvc (Device.Memory mem) mac

write :: Foreign.Storable.Generic.G v =>
	Device.D -> Device.MemoryAtom v -> M.MapFlags -> v -> IO ()
write dvc  (Device.MemoryAtom m) flgs v = do
	mem <- Device.Memory <$> newIORef m
	dat <- M.map dvc mem 0 (fromIntegral $ sizeOf v') flgs
	poke dat v'
	M.unmap dvc mem
	where v' = Foreign.Storable.Generic.Wrap v

read :: forall v . Foreign.Storable.Generic.G v =>
	Device.D -> Device.MemoryAtom v -> M.MapFlags -> IO v
read dvc (Device.MemoryAtom m) flgs = do
	mem <- Device.Memory <$> newIORef m
	dat <- M.map dvc mem 0 sz flgs
	Foreign.Storable.Generic.unWrap <$> peek dat <* M.unmap dvc mem
	where sz = fromIntegral (sizeOf @(Foreign.Storable.Generic.Wrap v) undefined)
