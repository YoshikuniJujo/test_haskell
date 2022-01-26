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

pipelineViewportStateCreateInfo :: #{type VkStructureType}
pipelineViewportStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO}

pipelineRasterizationStateCreateInfo :: #{type VkStructureType}
pipelineRasterizationStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO}

pipelineMultisampleStateCreateInfo :: #{type VkStructureType}
pipelineMultisampleStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO}
