{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory.List.Middle where

import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Pointable
import Control.Arrow
import Control.Exception
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
writeList dvc (toMemory -> (ln, mem)) flgs vs = bracket
	(M.map dvc mem 0 (fromIntegral $ sizeOf (vs' !! 0) * length vs') flgs)
	(const $ M.unmap dvc mem) (`pokeArray` take ln vs')
	where vs' = Foreign.Storable.Generic.Wrap <$> vs

writeMono :: (Foreign.Storable.Generic.G (Element vs), MonoFoldable vs) =>
	Device.D -> Device.MemoryList (Element vs) -> M.MapFlags -> vs -> IO ()
writeMono dvc (toMemory -> (ln, mem)) flgs vs = bracket
	(M.map dvc mem 0
		(fromIntegral $ sizeOf (w $ headEx vs) * olength vs) flgs)
	(const $ M.unmap dvc mem)
	(`pokeArray` (w <$> take ln (otoList vs)))
	where w = Foreign.Storable.Generic.Wrap

writeMonoW :: (Foreign.Storable.Generic.G v, MonoFoldable vs, Element vs ~ Foreign.Storable.Generic.Wrap v) =>
	Device.D -> Device.MemoryList v -> M.MapFlags -> vs -> IO ()
writeMonoW dvc (toMemory -> (ln, mem)) flgs vs = bracket
	(M.map dvc mem 0 (fromIntegral $ sizeOf (headEx vs) * olength vs) flgs)
	(const $ M.unmap dvc mem)
	(`pokeArray` take ln (otoList vs))

toMemory :: Device.MemoryList v -> (Int, Device.Memory)
toMemory = second Device.Memory . \(Device.MemoryList l m) -> (l, m)

readList :: forall v . Foreign.Storable.Generic.G v =>
	Device.D -> Device.MemoryList v -> M.MapFlags -> IO [v]
readList dvc (toMemory -> (ln, mem)) flgs = (Foreign.Storable.Generic.unWrap <$>) <$> bracket
	(M.map dvc mem 0 (fromIntegral $ sizeOf @(Foreign.Storable.Generic.Wrap v) undefined * ln) flgs)
	(const $ M.unmap dvc mem) (peekArray ln)
