{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueueFamily.EnumManual where

import Foreign.Storable
import Data.Word

#include <vulkan/vulkan.h>

import Foreign.C.Enum

enum "Index" ''#{type uint32_t} [''Show, ''Eq, ''Storable, ''Enum]
	[("Ignored", #{const VK_QUEUE_FAMILY_IGNORED})]

indices :: [Index]
indices = [Index 0 ..]
