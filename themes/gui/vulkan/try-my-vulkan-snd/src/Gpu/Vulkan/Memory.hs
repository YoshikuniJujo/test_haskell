{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory (

	-- * ALLOCATE AND BIND

	allocateBind, reallocateBind, M, AllocateInfo(..),
	ImageBuffer(..), ImageBufferBinded(..), ImageBufferArg(..),
	Alignments,

	-- * GET REQUREMENTS

	getRequirementsList, M.Requirements(..),

	-- * READ AND WRITE

	read, write, OffsetSize,

	-- * BARRIER

	M.Barrier(..),

	) where

import Prelude hiding (map, read)

import Foreign.Ptr
import Foreign.Storable.PeekPoke
import Control.Exception hiding (try)
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.Maybe
import qualified Data.HeteroParList as HeteroParList

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Device.Middle as Device.M
import qualified Gpu.Vulkan.Memory.Middle as Memory.M
import qualified Gpu.Vulkan.Memory.Middle as M

import Gpu.Vulkan.Memory.Bind
import Gpu.Vulkan.Memory.OffsetSize
import Gpu.Vulkan.Memory.Type
import Gpu.Vulkan.Memory.ImageBuffer

allocateBind :: (
	WithPoked (TMaybe.M mn),
	BindAll ibargs ibargs, Alignments ibargs,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
	AllocateInfo mn -> TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s .
		HeteroParList.PL (U2 (ImageBufferBinded s)) ibargs ->
		M s ibargs -> IO a) -> IO a
allocateBind dvc bs ai macc f = allocate dvc bs ai macc \m -> do
	bnds <- bindAll dvc bs m 0
	f bnds m

reallocateBind :: (
	WithPoked (TMaybe.M n), RebindAll ibargs ibargs, Alignments ibargs,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
	AllocateInfo n ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> M sm ibargs -> IO ()
reallocateBind dvc bs ai macc mem = do
	reallocate dvc bs ai macc mem
	rebindAll dvc bs mem 0

allocate :: (
	WithPoked (TMaybe.M n), Alignments ibargs,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd ->
	HeteroParList.PL (U2 ImageBuffer) ibargs ->
	AllocateInfo n ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . M s ibargs -> IO a) -> IO a
allocate dvc@(Device.D mdvc) bs ai
	(AllocationCallbacks.toMiddle -> mac) f = bracket
	do	mai <- allocateInfoToMiddle dvc bs ai
		Memory.M.allocate mdvc mai mac
	(\mem -> Memory.M.free mdvc mem mac)
	\mem -> f =<< newM2' bs mem

reallocate :: (
	WithPoked (TMaybe.M n), Alignments ibargs,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
	AllocateInfo n ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc -> M sm ibargs -> IO ()
reallocate dvc@(Device.D mdvc) bs ai
	(AllocationCallbacks.toMiddle -> mac) mem = do
	mai <- reallocateInfoToMiddle dvc bs ai
	(_, oldmem) <- readM mem
	Memory.M.reallocate mdvc mai mac oldmem
	writeMBinded' mem bs

data AllocateInfo mn = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoMemoryTypeIndex :: M.TypeIndex }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn)
deriving instance Eq (TMaybe.M mn) => Eq (AllocateInfo mn)

-- deriving instance Show (HeteroParList.PL (U2 ImageBuffer) ibargs) =>
--	Show (M s ibargs)

allocateInfoToMiddle :: forall sd ibargs n . Alignments ibargs =>
	Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
	AllocateInfo n -> IO (Memory.M.AllocateInfo n)
allocateInfoToMiddle dvc ibs AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoMemoryTypeIndex = mti } = do
	reqss <- getRequirementsList dvc ibs
	pure Memory.M.AllocateInfo {
		Memory.M.allocateInfoNext = mnxt,
		Memory.M.allocateInfoAllocationSize =
			memoryRequirementsListToSize 0
				(alignments @ibargs) reqss,
		Memory.M.allocateInfoMemoryTypeIndex = mti }

reallocateInfoToMiddle :: forall sd sm ibargs n . Alignments ibargs =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
	AllocateInfo n -> IO (Memory.M.AllocateInfo n)
reallocateInfoToMiddle dvc ibs AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoMemoryTypeIndex = mti } = do
	reqss <- getRequirementsListBinded dvc ibs
	pure Memory.M.AllocateInfo {
		Memory.M.allocateInfoNext = mnxt,
		Memory.M.allocateInfoAllocationSize =
			memoryRequirementsListToSize 0
				(alignments @ibargs) reqss,
		Memory.M.allocateInfoMemoryTypeIndex = mti }

memoryRequirementsListToSize ::
	Device.M.Size -> [Maybe Int] -> [Memory.M.Requirements] -> Device.M.Size
memoryRequirementsListToSize sz0 _ [] = sz0
memoryRequirementsListToSize sz0 [] _ = sz0
memoryRequirementsListToSize sz0 (malgn : malgns) (reqs : reqss) =
	memoryRequirementsListToSize
		(((sz0 - 1) `div` algn + 1) * algn + sz) malgns reqss
	where
	sz = Memory.M.requirementsSize reqs
--	algn = Memory.M.requirementsAlignment reqs
	algn = fromIntegral (fromMaybe 1 malgn) `lcm`
		Memory.M.requirementsAlignment reqs

write :: forall nm obj sd sm ibargs v .
	(VObj.StoreObject v obj, OffsetSize nm obj ibargs) =>
	Device.D sd -> M sm ibargs -> Memory.M.MapFlags -> v -> IO ()
write dvc mem flgs v = bracket
	(map @nm @obj dvc mem flgs) (const $ unmap dvc mem)
	(\(ptr :: Ptr (VObj.TypeOfObject obj)) -> do
		ln <- objectLength @nm @obj mem
		VObj.storeObject @_ @obj ptr ln v)

read :: forall nm obj v sd sm ibargs .
	(VObj.StoreObject v obj, OffsetSize nm obj ibargs) =>
	Device.D sd -> M sm ibargs -> Memory.M.MapFlags -> IO v
read dvc mem flgs = bracket
	(map @nm @obj dvc mem flgs) (const $ unmap dvc mem)
	(\(ptr :: Ptr (VObj.TypeOfObject obj)) ->
		VObj.loadObject @_ @obj ptr =<< objectLength @nm @obj mem)

map :: forall nm obj sd sm ibargs . OffsetSize nm obj ibargs =>
	Device.D sd -> M sm ibargs -> Memory.M.MapFlags -> IO (Ptr (VObj.TypeOfObject obj))
map dvc@(Device.D mdvc) m flgs = do
	(_, mm) <- readM m
	(ost, sz) <- offsetSize @nm @obj dvc m 0
--	putStrLn "Vk.Memory.map:"
--	putStr "ost: "; print ost
--	putStr "sz : "; print sz
	Memory.M.map mdvc mm ost sz flgs

unmap :: Device.D sd -> M sm ibargs -> IO ()
unmap (Device.D mdvc) m = do
	(_, mm) <- readM m
	Memory.M.unmap mdvc mm
