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

pipelineDepthStencilStateCreateInfo :: #{type VkStructureType}
pipelineDepthStencilStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO}

pipelineColorBlendStateCreateInfo :: #{type VkStructureType}
pipelineColorBlendStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO}

pipelineDynamicStateCreateInfo :: #{type VkStructureType}
pipelineDynamicStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO}

pipelineLayoutCreateInfo :: #{type VkStructureType}
pipelineLayoutCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO}

descriptorSetLayoutCreateInfo :: #{type VkStructureType}
descriptorSetLayoutCreateInfo =
	#{const VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO}

samplerCreateInfo :: #{type VkStructureType}
samplerCreateInfo = #{const VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO}

renderPassCreateInfo :: #{type VkStructureType}
renderPassCreateInfo = #{const VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO}

pipelineCreateInfo :: #{type VkStructureType}
pipelineCreateInfo = #{const VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO}

pipelineTessellationStateCreateInfo :: #{type VkStructureType}
pipelineTessellationStateCreateInfo =
	#{const VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO}

pipelineCacheCreateInfo :: #{type VkStructureType}
pipelineCacheCreateInfo = #{const VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO}

framebufferCreateInfo :: #{type VkStructureType}
framebufferCreateInfo = #{const VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO}

commandPoolCreateInfo :: #{type VkStructureType}
commandPoolCreateInfo = #{const VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO}

commandBufferAllocateInfo :: #{type VkStructureType}
commandBufferAllocateInfo = #{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO}

commandBufferInheritanceInfo :: #{type VkStructureType}
commandBufferInheritanceInfo =
	#{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO}

commandBufferBeginInfo :: #{type VkStructureType}
commandBufferBeginInfo = #{const VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO}

renderPassBeginInfo :: #{type VkStructureType}
renderPassBeginInfo = #{const VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO}

semaphoreCreateInfo :: #{type VkStructureType}
semaphoreCreateInfo = #{const VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO}

fenceCreateInfo :: #{type VkStructureType}
fenceCreateInfo = #{const VK_STRUCTURE_TYPE_FENCE_CREATE_INFO}

submitInfo :: #{type VkStructureType}
submitInfo = #{const VK_STRUCTURE_TYPE_SUBMIT_INFO}

presentInfoKhr :: #{type VkStructureType}
presentInfoKhr = #{const VK_STRUCTURE_TYPE_PRESENT_INFO_KHR}
