{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory where

import Data.Word

import Vulkan.Memory.Enum

import {-# SOURCE #-} qualified Vulkan.Device as Device
import qualified Vulkan.Memory.Core as C

#include <vulkan/vulkan.h>

newtype TypeBits = TypeBits #{type uint32_t} deriving Show

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
