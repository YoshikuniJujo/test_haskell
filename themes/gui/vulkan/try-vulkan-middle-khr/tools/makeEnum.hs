{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import qualified VulkanKhrSurfaceEnum
import qualified VulkanKhrSwapchainEnum

main :: IO ()
main = do
	VulkanKhrSurfaceEnum.make
	VulkanKhrSwapchainEnum.make
