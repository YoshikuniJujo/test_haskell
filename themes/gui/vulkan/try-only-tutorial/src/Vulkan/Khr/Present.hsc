{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Present where

import Data.Word

#include <vulkan/vulkan.h>

type Mode = #{type VkPresentModeKHR}

modeMailbox, modeFifo :: Mode
modeMailbox = #{const VK_PRESENT_MODE_MAILBOX_KHR}
modeFifo = #{const VK_PRESENT_MODE_FIFO_KHR}
