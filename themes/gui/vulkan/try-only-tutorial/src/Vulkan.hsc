{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan where

import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

newtype ApiVersion = ApiVersion #{type uint32_t}
	deriving (Show, Eq, Ord, Storable)

struct "ApplicationInfo" #{size VkApplicationInfo}
		#{alignment VkApplicationInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkApplicationInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_APPLICATION_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkApplicationInfo, pNext} |],
		[| #{poke VkApplicationInfo, pNext} |]),
	("pApplicationName", ''CString,
		[| #{peek VkApplicationInfo, pApplicationName} |],
		[| #{poke VkApplicationInfo, pApplicationName} |]),
	("applicationVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, applicationVersion} |],
		[| #{poke VkApplicationInfo, applicationVersion} |]),
	("pEngineName", ''CString,
		[| #{peek VkApplicationInfo, pEngineName} |],
		[| #{poke VkApplicationInfo, pEngineName} |]),
	("engineVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, engineVersion} |],
		[| #{poke VkApplicationInfo, engineVersion} |]),
	("apiVersion", ''ApiVersion,
		[| #{peek VkApplicationInfo, apiVersion} |],
		[| #{poke VkApplicationInfo, apiVersion} |])
	]
	[''Show]

foreign import capi "vulkan/vulkan.h VK_MAKE_API_VERSION" makeApiVersion ::
	Word8 -> Word8 -> Word8 -> Word16 -> ApiVersion
