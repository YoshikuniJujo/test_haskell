{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Framebuffer.Internal where

import Foreign.Storable
import Foreign.C.Struct

import Vulkan.Base

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkFramebufferCreateInfo}
		#{alignment VkFramebufferCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkFramebufferCreateInfo, sType}
			p ST.framebufferCreateInfo |]),
	("pNext", ''PtrVoid, [| #{peek VkFramebufferCreateInfo, pNext} |],
		[| #{poke VkFramebufferCreateInfo, pNext} |])
	]
	[''Show, ''Storable]
