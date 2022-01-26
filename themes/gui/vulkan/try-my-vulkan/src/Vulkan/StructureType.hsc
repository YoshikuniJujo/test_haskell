{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.StructureType where

import Data.Word

#include <vulkan/vulkan.h>

pipelineVertexInputStateCreateInfo :: #{type VkStructureType}
pipelineVertexInputStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO}

pipelineInputAssemblyStateCreateInfo :: #{type VkStructureType}
pipelineInputAssemblyStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO}
