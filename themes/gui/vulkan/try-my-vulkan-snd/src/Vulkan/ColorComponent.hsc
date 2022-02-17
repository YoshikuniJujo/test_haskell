{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.ColorComponent where

import Data.Word

#include <vulkan/vulkan.h>

rBit, gBit, bBit, aBit :: #{type VkColorComponentFlagBits}
rBit = #{const VK_COLOR_COMPONENT_R_BIT}
gBit = #{const VK_COLOR_COMPONENT_G_BIT}
bBit = #{const VK_COLOR_COMPONENT_B_BIT}
aBit = #{const VK_COLOR_COMPONENT_A_BIT}
