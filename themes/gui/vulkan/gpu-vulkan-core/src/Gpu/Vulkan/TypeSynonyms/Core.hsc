{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.TypeSynonyms.Core (

	-- * PTR

	PtrResult, PtrUint32T,

	-- * LIST

	ListUint8T, ListUint32T

	) where

import Foreign.Ptr
import Data.Word
import Data.Int

#include <vulkan/vulkan.h>

type PtrUint32T = Ptr #{type uint32_t}
type PtrResult = Ptr #{type VkResult}

type ListUint8T = [#{type uint8_t}]
type ListUint32T = [#{type uint32_t}]
