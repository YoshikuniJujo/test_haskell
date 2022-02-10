{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline where

import Data.Word

#include <vulkan/vulkan.h>

bindPointGraphics :: #{type VkPipelineBindPoint}
bindPointGraphics = #{const VK_PIPELINE_BIND_POINT_GRAPHICS}
