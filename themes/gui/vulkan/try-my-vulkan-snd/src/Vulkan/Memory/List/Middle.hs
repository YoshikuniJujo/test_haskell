{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.List.Middle where

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Data.MonoTraversable

import qualified Foreign.Storable.Generic

import Vulkan.Memory

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Middle as Device
import qualified Vulkan.Buffer.List.Middle as Buffer
import qualified Vulkan.Memory.Middle as M

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoMemoryTypeIndex :: TypeIndex }
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
allocate dvc b@(Buffer.B ln _) ai mac = do
	mai <- allocateInfoToMiddle dvc b ai
	(\(Device.Memory m) -> Device.MemoryList ln m) <$> M.allocate dvc mai mac

free :: Pointable n =>
	Device.D -> Device.MemoryList v -> Maybe (AllocationCallbacks.A n) ->
	IO ()
free dvc (Device.MemoryList _ m) mac = M.free dvc (Device.Memory m) mac

writeList :: Foreign.Storable.Generic.G v =>
	Device.D -> Device.MemoryList v -> M.MapFlags -> [v] -> IO ()
writeList dvc (Device.Memory . (\(Device.MemoryList _ m) -> m) -> mem) flgs vs = do
	dat <- M.map dvc mem 0
		(fromIntegral $ sizeOf (vs' !! 0) * length vs') flgs
	pokeArray dat vs'
	M.unmap dvc mem
	where vs' = Foreign.Storable.Generic.Wrap <$> vs

writeMono :: (Foreign.Storable.Generic.G (Element vs), MonoFoldable vs) =>
	Device.D -> Device.MemoryList (Element vs) -> M.MapFlags -> vs -> IO ()
writeMono dvc (Device.Memory . (\(Device.MemoryList _ m) -> m) -> mem) flgs vs = do
	dat <- M.map dvc mem 0
		(fromIntegral $ sizeOf (w $ headEx vs) * olength vs) flgs
	pokeArray dat $ w <$> otoList vs
	M.unmap dvc mem
	where w = Foreign.Storable.Generic.Wrap

writeMonoW :: (Foreign.Storable.Generic.G v, MonoFoldable vs, Element vs ~ Foreign.Storable.Generic.Wrap v) =>
	Device.D -> Device.MemoryList v -> M.MapFlags -> vs -> IO ()
writeMonoW dvc (Device.Memory . (\(Device.MemoryList _ m) -> m) -> mem) flgs vs = do
	dat <- M.map dvc mem 0
		(fromIntegral $ sizeOf (headEx vs) * olength vs) flgs
	pokeArray dat $ otoList vs
	M.unmap dvc mem

{-
readList ::
	Device.D -> Device.MemoryList v -> M.MapFlags -> IO [v]
	-}
