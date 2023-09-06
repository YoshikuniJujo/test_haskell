{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Access where

import Data.Word

#include <vulkan/vulkan.h>

colorAttachmentWriteBit :: #{type VkAccessFlagBits}
colorAttachmentWriteBit = #{const VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT}
