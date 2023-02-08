{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Data.Middle where

import Data.Word

#include <vulkan/vulkan.h>

uint64Max :: #{type uint64_t}
uint64Max = #{const UINT64_MAX}
