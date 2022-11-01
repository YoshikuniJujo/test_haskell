{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Middle.Internal (
	M(..), mToCore, AllocateInfo(..), allocate, reallocate, free,
	MapFlags(..), map, unmap,

	Requirements(..), requirementsFromCore,
	Barrier(..), barrierToCore,

	Heap, heapFromCore,

	MType(..), mTypeFromCore,
	TypeBits, TypeIndex, elemTypeIndex, typeBitsToTypeIndices
	) where

import Prelude hiding (map)

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc hiding (free)
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Monad.Cont
import Data.Default
import Data.Bits
import Data.IORef
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Memory.Enum

import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Memory.Core as C

#include <vulkan/vulkan.h>

newtype M = M (IORef C.M)

mToCore :: M -> IO C.M
mToCore (M r) = readIORef r

newtype TypeBits = TypeBits #{type uint32_t} deriving (Show, Eq, Bits, FiniteBits)

newtype TypeIndex = TypeIndex Word32
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

typeBitsToTypeIndices :: TypeBits -> [TypeIndex]
typeBitsToTypeIndices bs = (fst <$>)
	. filter snd . zip [0 ..] $ testBit bs <$> [0 .. finiteBitSize bs - 1]

elemTypeIndex :: TypeIndex -> TypeBits -> Bool
elemTypeIndex ti tbs = testBit tbs $ fromIntegral ti

data Requirements = Requirements {
	requirementsSize :: Device.Size,
	requirementsAlignment :: Device.Size,
	requirementsMemoryTypeBits :: TypeBits }
	deriving Show

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
	allocateInfoMemoryTypeIndex :: TypeIndex }
	deriving Show

allocateInfoToCore :: Pointable n =>
	AllocateInfo n -> ContT r IO (Ptr C.AllocateInfo)
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoAllocationSize = Device.Size sz,
	allocateInfoMemoryTypeIndex = TypeIndex mti } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	let	C.AllocateInfo_ fai = C.AllocateInfo {
			C.allocateInfoSType = (),
			C.allocateInfoPNext = pnxt,
			C.allocateInfoAllocationSize = sz,
			C.allocateInfoMemoryTypeIndex = mti }
	ContT $ withForeignPtr fai

allocate :: (Pointable n, Pointable n') =>
	Device.D -> AllocateInfo n -> Maybe (AllocationCallbacks.A n') ->
	IO M
allocate (Device.D dvc) ai mac = (M <$>) . ($ pure) $ runContT do
	pai <- allocateInfoToCore ai
	pac <- AllocationCallbacks.maybeToCore mac
	pm <- ContT alloca
	lift do	r <- C.allocate dvc pai pac pm
		throwUnlessSuccess $ Result r
		newIORef =<< peek pm

reallocate :: (Pointable n, Pointable c, Pointable d) =>
	Device.D -> AllocateInfo n ->
	Maybe (AllocationCallbacks.A c) ->
	Maybe (AllocationCallbacks.A d) ->
	M -> IO ()
reallocate d@(Device.D dvc) ai macc macd m@(M rm) = ($ pure) $ runContT do
	pai <- allocateInfoToCore ai
	pac <- AllocationCallbacks.maybeToCore macc
	pm <- ContT alloca
	lift do	r <- C.allocate dvc pai pac pm
		throwUnlessSuccess $ Result r
		free d m macd
		writeIORef rm =<< peek pm

free :: Pointable n =>
	Device.D -> M -> Maybe (AllocationCallbacks.A n) -> IO ()
free (Device.D dvc) (M mem) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	m <- lift $ readIORef mem
	lift $ C.free dvc m pac

enum "MapFlags" ''#{type VkMemoryMapFlags}
	[''Eq, ''Show, ''Storable, ''Bits] [("MapFlagsZero", 0)]

instance Default MapFlags where def = MapFlagsZero

map :: Device.D -> M -> Device.Size -> Device.Size -> MapFlags ->
	IO (Ptr a)
map (Device.D dvc) (M mem)
	(Device.Size ofst) (Device.Size sz) (MapFlags flgs) = ($ pure) $ runContT do
	pd <- ContT alloca
	m <- lift $ readIORef mem
	lift do	r <- C.map dvc m ofst sz flgs pd
		throwUnlessSuccess $ Result r
		peek pd

unmap :: Device.D -> M -> IO ()
unmap (Device.D dvc) (M mem) = C.unmap dvc =<< readIORef mem

data Barrier n = Barrier {
	barrierNext :: Maybe n,
	barrierSrcAccessMask :: AccessFlags,
	barrierDstAccessMask :: AccessFlags }
	deriving Show

barrierToCore :: Pointable n => Barrier n -> ContT r IO C.Barrier
barrierToCore Barrier {
	barrierNext = mnxt,
	barrierSrcAccessMask = AccessFlagBits sam,
	barrierDstAccessMask = AccessFlagBits dam } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pure C.Barrier {
		C.barrierSType = (),
		C.barrierPNext = pnxt,
		C.barrierSrcAccessMask = sam,
		C.barrierDstAccessMask = dam }
