{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.FrontFace where

import Data.Word

#include <vulkan/vulkan.h>

clockwise :: #{type VkFrontFace}
clockwise = #{const VK_FRONT_FACE_CLOCKWISE}
