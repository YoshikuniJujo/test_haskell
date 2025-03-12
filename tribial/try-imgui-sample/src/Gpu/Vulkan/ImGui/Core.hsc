{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Core (
	checkVersion,
	createContextNoArg, Context,
	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Gpu.Vulkan.Instance.Core qualified as Vk.Ist

#include "imgui_c.h"

checkVersion :: IO ()
checkVersion = cxx_imgui_check_version

createContextNoArg :: IO Context
createContextNoArg = cxx_create_context_no_arg

newtype Context = Context (Ptr ContextTag)
data ContextTag

foreign import ccall "imgui_check_version" cxx_imgui_check_version :: IO ()
foreign import ccall "create_context_no_arg" cxx_create_context_no_arg :: IO Context

struct "InitInfo" #{size struct ImGui_ImplVulkan_InitInfo}
	#{alignment struct ImGui_ImplVulkan_InitInfo} [
	("ApiVersion", ''#{type uint32_t},
		[| #{peek struct ImGui_ImplVulkan_InitInfo, ApiVersion} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, ApiVersion} |]),
	("Instance", ''Vk.Ist.I,
		[| #{peek struct ImGui_ImplVulkan_InitInfo, Instance} |],
		[| #{poke struct ImGui_ImplVulkan_InitInfo, Instance} |])
	]
	[''Show, ''Storable]
