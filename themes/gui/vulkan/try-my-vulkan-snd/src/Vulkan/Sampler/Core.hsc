{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Sampler.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

data STag
type S = Ptr STag

type PtrS = Ptr S

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO}

struct "CreateInfo" #{size VkSamplerCreateInfo}
		#{alignment VkSamplerCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkSamplerCreateInfo, sType} p sType |]),
	("pNext", ''PtrVoid,
		[| #{peek VkSamplerCreateInfo, pNext} |],
		[| #{poke VkSamplerCreateInfo, pNext} |])
	]
	[''Show, ''Storable]
