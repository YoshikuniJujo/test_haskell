{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified VulkanEnum
import qualified VulkanExceptionEnum

import qualified VulkanExtDebugUtilsMessageEnum
import qualified VulkanPhysicalDeviceEnum
import qualified VulkanSampleEnum
import qualified VulkanDeviceQueueEnum
import qualified VulkanKhrSurfaceEnum
import qualified VulkanKhrEnum
import qualified VulkanImageEnum
import qualified VulkanKhrSwapchainEnum
import qualified VulkanImageViewEnum
import qualified VulkanComponentEnum

import qualified VulkanPipelineShaderStageEnum
import qualified VulkanShaderStageEnum
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

main :: IO ()
main = do
	VulkanEnum.make
	VulkanExceptionEnum.make

	VulkanExtDebugUtilsMessageEnum.make
	VulkanPhysicalDeviceEnum.make
	VulkanSampleEnum.make
	VulkanDeviceQueueEnum.make
	VulkanKhrSurfaceEnum.make
	VulkanKhrEnum.make
	VulkanImageEnum.make
	VulkanKhrSwapchainEnum.make
	VulkanImageViewEnum.make
	VulkanComponentEnum.make

	VulkanPipelineShaderStageEnum.make
	VulkanShaderStageEnum.make
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
