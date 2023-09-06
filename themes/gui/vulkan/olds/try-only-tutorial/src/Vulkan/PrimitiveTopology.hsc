{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PrimitiveTopology where

import Data.Word

#include <vulkan/vulkan.h>

triangleList :: #{type VkPrimitiveTopology}
triangleList = #{const VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST}
