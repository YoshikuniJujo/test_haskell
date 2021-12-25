{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext where

#include <vulkan/vulkan.h>

debugUtilsExtensionName :: String
debugUtilsExtensionName = #{const_str VK_EXT_DEBUG_UTILS_EXTENSION_NAME}
