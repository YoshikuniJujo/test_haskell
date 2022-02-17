{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Polygon where

import Data.Word

#include <vulkan/vulkan.h>

modeFill :: #{type VkPolygonMode}
modeFill = #{const VK_POLYGON_MODE_FILL}
