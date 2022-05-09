{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Pointable
import Control.Monad.Cont
import Data.Bits
import Data.Word

import Vulkan.Exception
import Vulkan.Exception.Enum
import Vulkan.Memory.Enum

import {-# SOURCE #-} qualified Vulkan.Device as Device

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Memory.Core as C

#include <vulkan/vulkan.h>

newtype TypeBits = TypeBits #{type uint32_t} deriving (Show, Eq, Bits)

data Requirements = Requirements {
	requirementsSize :: Device.Size,
	requirementsAlignment :: Device.Size,
	requirementsMemoryTypeBits :: TypeBits }
	deriving Show

requirementsToCore :: Requirements -> C.Requirements
requirementsToCore Requirements {
	requirementsSize = Device.Size sz,
	requirementsAlignment = Device.Size al,
	requirementsMemoryTypeBits = TypeBits mtbs } = C.Requirements {
	C.requirementsSize = sz,
	C.requirementsAlignment = al,
	C.requirementsMemoryTypeBits = mtbs }

requirementsFromCore :: C.Requirements -> Requirements
requirementsFromCore C.Requirements {
	C.requirementsSize = sz,
	C.requirementsAlignment = al,
	C.requirementsMemoryTypeBits = mtbs } = Requirements {
	requirementsSize = Device.Size sz,
	requirementsAlignment = Device.Size al,
	requirementsMemoryTypeBits = TypeBits mtbs }

data MType = MType {
	mTypePropertyFlags :: PropertyFlags,
	mTypeHeapIndex :: #{type uint32_t} }
	deriving Show

mTypeFromCore :: C.MType -> MType
mTypeFromCore C.MType {
	C.mTypePropertyFlags = pfs,
	C.mTypeHeapIndex = hi } = MType {
	mTypePropertyFlags = PropertyFlagBits pfs,
	mTypeHeapIndex = hi }

data Heap = Heap { heapSize :: Device.Size, heapFlags :: HeapFlags }
	deriving Show

heapFromCore :: C.Heap -> Heap
heapFromCore C.Heap { C.heapSize = sz, C.heapFlags = flgs } =
	Heap { heapSize = Device.Size sz, heapFlags = HeapFlagBits flgs }

data AllocateInfo n = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoAllocationSize :: Device.Size,
	allocateInfoMemoryTypeIndex :: #{type uint32_t} }
	deriving Show

allocateInfoToCore :: Pointable n =>
	AllocateInfo n -> ContT r IO (Ptr C.AllocateInfo)
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoAllocationSize = Device.Size sz,
	allocateInfoMemoryTypeIndex = mti } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.AllocateInfo_ fai = C.AllocateInfo {
			C.allocateInfoSType = (),
			C.allocateInfoPNext = pnxt,
			C.allocateInfoAllocationSize = sz,
			C.allocateInfoMemoryTypeIndex = mti }
	ContT $ withForeignPtr fai

allocate :: (Pointable n, Pointable n') =>
	Device.D -> AllocateInfo n -> Maybe (AllocationCallbacks.A n') ->
	IO Device.Memory
allocate (Device.D dvc) ai mac =  (Device.Memory <$>) . ($ pure) $ runContT do
	pai <- allocateInfoToCore ai
	pac <- AllocationCallbacks.maybeToCore mac
	pm <- ContT alloca
	lift do	r <- C.allocate dvc pai pac pm
		throwUnlessSuccess $ Result r
		peek pm
