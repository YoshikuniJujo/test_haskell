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

	allocateBind, reallocateBind,

	-- ** MEMORY

	M, ImageBuffer(..), ImageBufferBinded(..), ImageBufferArg(..),

	-- ** ALLOCATE INFO

	AllocateInfo(..), M.MType(..), M.TypeBits, M.TypeIndex, M.elemTypeIndex,

	-- ** BINDABLE AND REBINDABLE

	Bindable, Rebindable,

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
import qualified Gpu.Vulkan.Memory.Middle as M

import Gpu.Vulkan.Memory.Bind
import Gpu.Vulkan.Memory.OffsetSize
import Gpu.Vulkan.Memory.Type
import Gpu.Vulkan.Memory.ImageBuffer

-- ALLOCATE AND BIND

-- Allocate Bind

allocateBind :: (
	WithPoked (TMaybe.M mn), Bindable ibargs,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
	AllocateInfo mn -> TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . HeteroParList.PL (U2 (ImageBufferBinded s)) ibargs ->
		M s ibargs -> IO a) -> IO a
allocateBind dv ibs ai mac f =
	allocate dv ibs ai mac \m -> (`f` m) =<< bindAll dv ibs m 0

allocate :: (
	WithPoked (TMaybe.M n), Alignments ibargs,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
	AllocateInfo n -> TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . M s ibargs -> IO a) -> IO a
allocate dv@(Device.D mdv) ibs ai
	(AllocationCallbacks.toMiddle -> mac) f = bracket
	do	mai <- allocateInfoToMiddle dv ibs ai
		M.allocate mdv mai mac
	(\m -> M.free mdv m mac)
	\m -> f =<< newM ibs m

-- Reallocate Bind

reallocateBind :: (
	WithPoked (TMaybe.M mn), Rebindable ibargs,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
	AllocateInfo mn -> TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	M sm ibargs -> IO ()
reallocateBind dv ibs ai mac m =
	reallocate dv ibs ai mac m >> rebindAll dv ibs m 0

reallocate :: (
	WithPoked (TMaybe.M n), Alignments ibargs,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
	AllocateInfo n -> TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	M sm ibargs -> IO ()
reallocate dv@(Device.D mdv) ibs ai (AllocationCallbacks.toMiddle -> mac) m = do
	mai <- reallocateInfoToMiddle dv ibs ai
	(_, mm) <- readM m
	M.reallocate mdv mai mac mm
	writeMBinded m ibs

reallocate' :: (
	WithPoked (TMaybe.M n), Alignments ibargs,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
	AllocateInfo n -> TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	M sm ibargs -> IO a -> IO ()
reallocate' dv@(Device.D mdv) ibs ai (AllocationCallbacks.toMiddle -> mac) m act = do
	mai <- reallocateInfoToMiddle dv ibs ai
	(_, mm) <- readM m
	M.reallocate' mdv mai mac mm $ writeMBinded m ibs >> act

-- Allocate Info

data AllocateInfo mn = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoMemoryTypeIndex :: M.TypeIndex }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn)
deriving instance Eq (TMaybe.M mn) => Eq (AllocateInfo mn)

allocateInfoToMiddle :: forall sd ibargs mn . Alignments ibargs =>
	Device.D sd -> HeteroParList.PL (U2 ImageBuffer) ibargs ->
	AllocateInfo mn -> IO (M.AllocateInfo mn)
allocateInfoToMiddle dv ibs AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoMemoryTypeIndex = mti } = do
	reqss <- getRequirementsList dv ibs
	pure M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoAllocationSize = memoryRequirementsListToSize
			0 (alignments @ibargs) reqss,
		M.allocateInfoMemoryTypeIndex = mti }

reallocateInfoToMiddle :: forall sd sm ibargs mn . Alignments ibargs =>
	Device.D sd -> HeteroParList.PL (U2 (ImageBufferBinded sm)) ibargs ->
	AllocateInfo mn -> IO (M.AllocateInfo mn)
reallocateInfoToMiddle dv ibs AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoMemoryTypeIndex = mti } = do
	reqss <- getRequirementsListBinded dv ibs
	pure M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoAllocationSize = memoryRequirementsListToSize
			0 (alignments @ibargs) reqss,
		M.allocateInfoMemoryTypeIndex = mti }

memoryRequirementsListToSize ::
	Device.M.Size -> [Maybe Device.M.Size] -> [M.Requirements] -> Device.M.Size
memoryRequirementsListToSize sz0 _ [] = sz0
memoryRequirementsListToSize sz0 [] _ = sz0
memoryRequirementsListToSize sz0 (malgn : malgns) (reqs : reqss) =
	memoryRequirementsListToSize
		(((sz0 - 1) `div` algn + 1) * algn + sz) malgns reqss
	where
	sz = M.requirementsSize reqs
	algn = (fromMaybe 1 malgn) `lcm`
		M.requirementsAlignment reqs

-- READ AND WRITE

read :: forall nm obj v sd sm ibargs .
	(VObj.Store v obj, OffsetSize nm obj ibargs) =>
	Device.D sd -> M sm ibargs -> M.MapFlags -> IO v
read dv m flgs = bracket
	(map @nm @obj dv m flgs) (const $ unmap dv m)
	\(ptr :: Ptr (VObj.TypeOf obj)) ->
		VObj.load @_ @obj ptr =<< objectLength @nm @obj m

write :: forall nm obj sd sm ibargs v .
	(VObj.Store v obj, OffsetSize nm obj ibargs) =>
	Device.D sd -> M sm ibargs -> M.MapFlags -> v -> IO ()
write dv m flgs v = bracket
	(map @nm @obj dv m flgs) (const $ unmap dv m)
	\(ptr :: Ptr (VObj.TypeOf obj)) -> do
		ln <- objectLength @nm @obj m
		VObj.store @_ @obj ptr ln v

map :: forall nm obj sd sm ibargs . OffsetSize nm obj ibargs =>
	Device.D sd -> M sm ibargs -> M.MapFlags ->
	IO (Ptr (VObj.TypeOf obj))
map dv@(Device.D mdv) m flgs = readM m >>= \(_, mm) -> do
	(ost, sz) <- offsetSize @nm @obj dv m 0
	M.map mdv mm ost sz flgs

unmap :: Device.D sd -> M sm ibargs -> IO ()
unmap (Device.D mdv) m = M.unmap mdv . snd =<< readM m
