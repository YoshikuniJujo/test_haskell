-- This file is automatically generated by the tools/makeEnumVkCommandBufferUsageFlagBits.hs
--	% stack runghc --cwd tools/ makeEnumVkCommandBufferUsageFlagBits

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBufferUsageFlagBits where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word
import Data.Bits

#include <vulkan/vulkan.h>

enum "CommandBufferUsageFlagBits" ''#{type VkCommandBufferUsageFlagBits}
		[''Show, ''Eq, ''Storable, ''Bits] [
	("CommandBufferUsageFlagsZero", 0),
	("CommandBufferUsageOneTimeSubmitBit",
		#{const VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}),
	("CommandBufferUsageRenderPassContinueBit",
		#{const VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT}),
	("CommandBufferUsageSimultaneousUseBit",
		#{const VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT}),
	("CommandBufferUsageFlagBitsMaxEnum",
		#{const VK_COMMAND_BUFFER_USAGE_FLAG_BITS_MAX_ENUM}) ]

type CommandBufferUsageFlags = CommandBufferUsageFlagBits