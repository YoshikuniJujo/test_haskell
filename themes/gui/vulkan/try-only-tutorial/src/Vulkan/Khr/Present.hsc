{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Present where

import Data.Word

#include <vulkan/vulkan.h>

type Mode = #{type VkPresentModeKHR}
