{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.ColorSpace where

import Data.Word

#include <vulkan/vulkan.h>

srgbNonlinear :: #{type VkColorSpaceKHR}
srgbNonlinear = #{const VK_COLOR_SPACE_SRGB_NONLINEAR_KHR}
