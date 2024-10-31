{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Default
import Data.Bits
import Data.Word

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkInstanceCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

instance Default CreateFlags where def = CreateFlagsZero
