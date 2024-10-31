{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Data.Middle {-# DEPRECATED "use maxBound instesad of uint64Max" #-} where

import Data.Word

#include <vulkan/vulkan.h>

{-# DEPRECATED uint64Max "use maxBound :: Word64" #-}

uint64Max :: #{type uint64_t}
uint64Max = #{const UINT64_MAX}
