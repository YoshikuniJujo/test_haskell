{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Semaphore.Internal where

import Foreign.C.Enum
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkSemaphoreCreateFlags} [''Show, ''Storable]
	[("CreateFlagsZero", 0)]

struct "CreateInfo" #{size VkSemaphoreCreateInfo}
		#{alignment VkSemaphoreCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSemaphoreCreateInfo, sType}
			p ST.semaphoreCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkSemaphoreCreateInfo, pNext} |],
		[| #{poke VkSemaphoreCreateInfo, pNext} |]),
	("flags", ''CreateFlags, [| #{peek VkSemaphoreCreateInfo, flags} |],
		[| #{poke VkSemaphoreCreateInfo, flags} |]) ]
	[''Show, ''Storable]
