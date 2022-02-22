{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified VulkanEnum
import qualified VulkanExceptionEnum

import qualified VulkanExtDebugUtilsMessageEnum

import qualified VulkanPipelineShaderStageEnum

main :: IO ()
main = do
	VulkanEnum.make
	VulkanExceptionEnum.make

	VulkanExtDebugUtilsMessageEnum.make

	VulkanPipelineShaderStageEnum.make
