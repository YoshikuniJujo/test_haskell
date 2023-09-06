module Vulkan.Sample where

import Data.Word

#include <vulkan/vulkan.h>

count1Bit :: #{type VkSampleCountFlagBits}
count1Bit = #{const VK_SAMPLE_COUNT_1_BIT}
