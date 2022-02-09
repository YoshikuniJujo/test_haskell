{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Logic where

import Data.Word

#include <vulkan/vulkan.h>

opCopy :: #{type VkLogicOp}
opCopy = #{const VK_LOGIC_OP_COPY}
