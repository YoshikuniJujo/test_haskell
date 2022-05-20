{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.QueueFamily.EnumManual where

import Foreign.Storable
import Data.Word

#include <vulkan/vulkan.h>

import Foreign.C.Enum

enum "Index" ''#{type uint32_t} [''Show, ''Storable, ''Num]
	[("Ignored", #{const VK_QUEUE_FAMILY_IGNORED})]
