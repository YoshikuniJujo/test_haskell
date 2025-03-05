{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Gpu.Vulkan.ImGui.Helper.Core

main :: IO ()
main = do
	print cxx_sizeOfImguiImplVulkanHFrame
	print cxx_alignOfImguiImplVulkanHFrame
