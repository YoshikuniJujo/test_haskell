{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.RenderPass where

import Foreign.Storable
import Foreign.C.Struct
import Data.Word

#include <vulkan/vulkan.h>

sType :: #{type VkStructureType}
sType = #{const VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO}

struct "CreateInfo" #{size VkRenderPassCreateInfo}
		#{alignment VkRenderPassCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkRenderPassCreateInfo, sType} p sType |])
	]
	[''Show, ''Storable]
