{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Swapchain where

import qualified Data.Text as T

#include <vulkan/vulkan.h>

extensionName :: T.Text
extensionName = #{const_str VK_KHR_SWAPCHAIN_EXTENSION_NAME}
