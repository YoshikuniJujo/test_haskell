{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Swapchain where

#include <vulkan/vulkan.h>

swapchainExtensionName :: String
swapchainExtensionName = #{const_str VK_KHR_SWAPCHAIN_EXTENSION_NAME}
