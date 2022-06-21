{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Default
import Data.Word

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkInstanceCreateFlags} [''Show, ''Eq, ''Storable] [
	("CreateFlagsZero", 0) ]

instance Default CreateFlags where def = CreateFlagsZero
