-- This file is automatically generated by the tools/makeEnum.hs
--	% stack runghc --cwd tools/ makeEnum

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-export-lists -fno-warn-tabs #-}

module Gpu.Vulkan.Fence.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Bits
import Data.Word

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkFenceCreateFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("CreateFlagsZero", 0),
	("CreateSignaledBit", #{const VK_FENCE_CREATE_SIGNALED_BIT}),
	("CreateFlagBitsMaxEnum",
		#{const VK_FENCE_CREATE_FLAG_BITS_MAX_ENUM}) ]

type CreateFlags = CreateFlagBits
