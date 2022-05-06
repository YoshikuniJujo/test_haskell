{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Memory where

import Data.Word

import qualified Vulkan.Device as Device
import qualified Vulkan.Memory.Core as C

#include <vulkan/vulkan.h>

newtype MemoryTypeBits = MemoryTypeBits #{type uint32_t} deriving Show

data Requirements = Requirements {
	requirementsSize :: Device.Size,
	requirementsAlignment :: Device.Size,
	requirementsMemoryTypeBits :: MemoryTypeBits }
	deriving Show

requirementsToCore :: Requirements -> C.Requirements
requirementsToCore Requirements {
	requirementsSize = Device.Size sz,
	requirementsAlignment = Device.Size al,
	requirementsMemoryTypeBits = MemoryTypeBits mtbs } = C.Requirements {
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
	requirementsMemoryTypeBits = MemoryTypeBits mtbs }
