{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Enumerate.Core where

import Foreign.Ptr
import Foreign.C.String
import Data.Word
import Data.Int

import Vulkan.Core

#include <vulkan/vulkan.h>

foreign import ccall "vkEnumerateInstanceExtensionProperties"
	instanceExtensionProperties ::
	CString -> Ptr #{type uint32_t} -> Ptr ExtensionProperties ->
	IO #{type VkResult}

foreign import ccall "vkEnumerateInstanceLayerProperties"
	instanceLayerProperties ::
	Ptr #{type uint32_t} -> Ptr LayerProperties -> IO #{type VkResult}
