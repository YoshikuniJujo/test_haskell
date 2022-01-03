{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Swapchain where

import qualified Vulkan.Khr.Swapchain.Internal as I

#include <vulkan/vulkan.h>

swapchainExtensionName :: String
swapchainExtensionName = #{const_str VK_KHR_SWAPCHAIN_EXTENSION_NAME}
