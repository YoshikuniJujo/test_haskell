{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Format where

import Data.Word

#include <vulkan/vulkan.h>

b8g8r8a8Srgb :: #{type VkFormat}
b8g8r8a8Srgb = #{const VK_FORMAT_B8G8R8A8_SRGB}
