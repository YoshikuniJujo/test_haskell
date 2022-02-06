{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr where

import Data.Word

#include <vulkan/vulkan.h>

compositeAlphaOpaqueBit :: #{type VkCompositeAlphaFlagBitsKHR}
compositeAlphaOpaqueBit = #{const VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR}
