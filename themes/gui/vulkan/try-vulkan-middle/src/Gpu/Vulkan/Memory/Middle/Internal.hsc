{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
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
import Foreign.Marshal.Alloc hiding (free)
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Default
import Data.Bits
import Data.IORef
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Memory.Enum

import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as Device

import qualified Gpu.Vulkan.AllocationCallbacks.Middle.Internal as AllocationCallbacks
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

data AllocateInfo mn = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoAllocationSize :: Device.Size,
	allocateInfoMemoryTypeIndex :: TypeIndex }

deriving instance Show (TMaybe.M mn) => Show (AllocateInfo mn)

allocateInfoToCore :: WithPoked (TMaybe.M mn) =>
	AllocateInfo mn -> (Ptr C.AllocateInfo -> IO a) -> IO ()
allocateInfoToCore AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoAllocationSize = Device.Size sz,
	allocateInfoMemoryTypeIndex = TypeIndex mti } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	ci = C.AllocateInfo {
			C.allocateInfoSType = (),
			C.allocateInfoPNext = pnxt',
			C.allocateInfoAllocationSize = sz,
			C.allocateInfoMemoryTypeIndex = mti } in
	withPoked ci f

allocate :: (WithPoked (TMaybe.M mn), WithPoked a) =>
	Device.D -> AllocateInfo mn -> Maybe (AllocationCallbacks.A a) -> IO M
allocate (Device.D dvc) ai mac = M <$> alloca \pm -> do
	allocateInfoToCore ai \pai ->
		AllocationCallbacks.maybeToCore mac \pac -> do
			r <- C.allocate dvc pai pac pm
			throwUnlessSuccess $ Result r
	newIORef =<< peek pm

reallocate :: (WithPoked (TMaybe.M mn), WithPoked a, WithPoked f) =>
	Device.D -> AllocateInfo mn ->
	Maybe (AllocationCallbacks.A a) ->
	Maybe (AllocationCallbacks.A f) ->
	M -> IO ()
reallocate d@(Device.D dvc) ai macc macd m@(M rm) =
	alloca \pm -> allocateInfoToCore ai \pai ->
	AllocationCallbacks.maybeToCore macc \pac -> do
		r <- C.allocate dvc pai pac pm
		throwUnlessSuccess $ Result r
		free d m macd
		writeIORef rm =<< peek pm

free :: WithPoked f => Device.D -> M -> Maybe (AllocationCallbacks.A f) -> IO ()
free (Device.D dvc) (M mem) mac =
	AllocationCallbacks.maybeToCore mac \pac -> do
		m <- readIORef mem
		C.free dvc m pac

enum "MapFlags" ''#{type VkMemoryMapFlags}
	[''Eq, ''Show, ''Storable, ''Bits] [("MapFlagsZero", 0)]

instance Default MapFlags where def = MapFlagsZero

map :: Device.D -> M -> Device.Size -> Device.Size -> MapFlags ->
	IO (Ptr a)
map (Device.D dvc) (M mem)
	(Device.Size ofst) (Device.Size sz) (MapFlags flgs) = alloca \pd ->
	readIORef mem >>= \m ->
	C.map dvc m ofst sz flgs pd >>= \r ->
	throwUnlessSuccess (Result r) >>
	peek pd

unmap :: Device.D -> M -> IO ()
unmap (Device.D dvc) (M mem) = C.unmap dvc =<< readIORef mem

data Barrier mn = Barrier {
	barrierNext :: TMaybe.M mn,
	barrierSrcAccessMask :: AccessFlags,
	barrierDstAccessMask :: AccessFlags }

deriving instance Show (TMaybe.M mn) => Show (Barrier mn)

barrierToCore :: WithPoked (TMaybe.M mn) => Barrier mn -> (C.Barrier -> IO a) -> IO ()
barrierToCore Barrier {
	barrierNext = mnxt,
	barrierSrcAccessMask = AccessFlagBits sam,
	barrierDstAccessMask = AccessFlagBits dam } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> f C.Barrier {
		C.barrierSType = (),
		C.barrierPNext = pnxt',
		C.barrierSrcAccessMask = sam,
		C.barrierDstAccessMask = dam }
