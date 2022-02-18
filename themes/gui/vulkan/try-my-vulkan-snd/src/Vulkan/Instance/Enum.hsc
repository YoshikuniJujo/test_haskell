{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Instance.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkInstanceCreateFlags} [''Show, ''Eq, ''Storable] [
	("CreateFlagsZero", 0) ]
