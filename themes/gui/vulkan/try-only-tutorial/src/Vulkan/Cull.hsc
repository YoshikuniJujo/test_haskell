{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Cull where

import Data.Word

#include <vulkan/vulkan.h>

modeBackBit :: #{type VkCullModeFlagBits}
modeBackBit = #{const VK_CULL_MODE_BACK_BIT}
