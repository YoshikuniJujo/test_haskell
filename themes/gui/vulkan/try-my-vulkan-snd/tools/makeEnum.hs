{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified VulkanExtDebugUtilsMessageEnum
import qualified VulkanKhrSurfaceEnum
import qualified VulkanKhrEnum
import qualified VulkanKhrSwapchainEnum

main :: IO ()
main = do
	VulkanExtDebugUtilsMessageEnum.make
	VulkanKhrSurfaceEnum.make
	VulkanKhrEnum.make
	VulkanKhrSwapchainEnum.make
