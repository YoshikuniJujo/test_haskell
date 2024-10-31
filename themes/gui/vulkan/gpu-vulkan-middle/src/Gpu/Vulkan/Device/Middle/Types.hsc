{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle.Types where

import Foreign.C.Enum
import Data.Word
import Data.Ix
import Gpu.Vulkan.Device.Core qualified as C

#include <vulkan/vulkan.h>

newtype D = D C.D deriving Show

enum "Size" ''#{type VkDeviceSize}
		[''Show, ''Eq, ''Ord, ''Enum, ''Num, ''Real, ''Integral, ''Ix]
	[("WholeSize", #{const VK_WHOLE_SIZE})]
