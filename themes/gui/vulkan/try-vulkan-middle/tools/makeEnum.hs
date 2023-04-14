{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified VulkanEnum
import qualified VulkanExceptionEnum

import qualified VulkanPhysicalDeviceEnum
import qualified VulkanSampleEnum
import qualified VulkanDeviceQueueEnum
import qualified VulkanImageEnum
import qualified VulkanImageViewEnum
import qualified VulkanComponentEnum

import qualified VulkanPipelineShaderStageEnum
import qualified VulkanVertexInputEnum
import qualified VulkanColorComponentEnum
import qualified VulkanDescriptorSetLayoutEnum
import qualified VulkanAttachmentEnum
import qualified VulkanSubpassEnum
import qualified VulkanPipelineEnum
import qualified VulkanRenderPassEnum
import qualified VulkanPipelineCacheEnum
import qualified VulkanFramebufferEnum
import qualified VulkanCommandPoolEnum
import qualified VulkanCommandBufferEnum
import qualified VulkanFenceEnum
import qualified VulkanBufferEnum
import qualified VulkanMemoryEnum
import qualified VulkanDescriptorEnum
import qualified VulkanDescriptorPoolEnum
import qualified VulkanSamplerEnum

import qualified VulkanQueueEnum

import qualified VulkanQueryEnum

main :: IO ()
main = do
	VulkanEnum.make
	VulkanExceptionEnum.make

	VulkanPhysicalDeviceEnum.make
	VulkanSampleEnum.make
	VulkanDeviceQueueEnum.make
	VulkanImageEnum.make
	VulkanImageViewEnum.make
	VulkanComponentEnum.make

	VulkanPipelineShaderStageEnum.make
	VulkanVertexInputEnum.make
	VulkanColorComponentEnum.make
	VulkanDescriptorSetLayoutEnum.make
	VulkanAttachmentEnum.make
	VulkanSubpassEnum.make
	VulkanPipelineEnum.make
	VulkanRenderPassEnum.make
	VulkanPipelineCacheEnum.make
	VulkanFramebufferEnum.make
	VulkanCommandPoolEnum.make
	VulkanCommandBufferEnum.make
	VulkanFenceEnum.make
	VulkanBufferEnum.make
	VulkanMemoryEnum.make
	VulkanDescriptorEnum.make
	VulkanDescriptorPoolEnum.make
	VulkanSamplerEnum.make

	VulkanQueueEnum.make

	VulkanQueryEnum.make
