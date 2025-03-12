{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Core (
	checkVersion
	) where

checkVersion :: IO ()
checkVersion = cxx_imgui_check_version

foreign import ccall "imgui_check_version" cxx_imgui_check_version :: IO ()
