{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.DepthStencilState where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct

#include <vulkan/vulkan.h>

struct "CreateInfo" #{size VkPipelineDepthStencilStateCreateInfo}
		#{alignment VkPipelineDepthStencilStateCreateInfo} [
	-- TODO
	]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
