{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Blend where

import Data.Word

#include <vulkan/vulkan.h>

opAdd :: #{type VkBlendOp}
opAdd = #{const VK_BLEND_OP_ADD}

factorZero, factorOne :: #{type VkBlendFactor}
factorZero = #{const VK_BLEND_FACTOR_ZERO}
factorOne = #{const VK_BLEND_FACTOR_ONE}
