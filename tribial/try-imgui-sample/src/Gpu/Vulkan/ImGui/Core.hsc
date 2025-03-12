{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Core (
	checkVersion,
	createContextNoArg, Context,
	) where

import Foreign.Ptr

checkVersion :: IO ()
checkVersion = cxx_imgui_check_version

createContextNoArg :: IO Context
createContextNoArg = cxx_create_context_no_arg

newtype Context = Context (Ptr ContextTag)
data ContextTag

foreign import ccall "imgui_check_version" cxx_imgui_check_version :: IO ()
foreign import ccall "create_context_no_arg" cxx_create_context_no_arg :: IO Context
