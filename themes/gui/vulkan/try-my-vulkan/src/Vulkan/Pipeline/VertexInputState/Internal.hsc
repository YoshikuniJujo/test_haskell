{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.Internal where

import Foreign.Storable
import Foreign.C.Struct

import Vulkan.Base

import qualified Vulkan.StructureType as SType

#include <vulkan/vulkan.h>

struct "PipelineVertexInputStateCreateInfo"
		#{size VkPipelineVertexInputStateCreateInfo}
		#{alignment VkPipelineVertexInputStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineVertexInputStateCreateInfo, sType}
			p SType.pipelineVertexInputStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineVertexInputStateCreateInfo, pNext} |],
		[| #{poke VkPipelineVertexInputStateCreateInfo, pNext} |])
	]
	[''Show]
