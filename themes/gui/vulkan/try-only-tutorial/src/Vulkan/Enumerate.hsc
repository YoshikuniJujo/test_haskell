{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Enumerate where

import Foreign.Ptr
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Data.Word
import Data.Int

import qualified Data.ByteString as BS

#include <vulkan/vulkan.h>

struct "ExtensionProperties" #{size VkExtensionProperties}
		#{alignment VkExtensionProperties} [
	("extensionName", ''BS.ByteString,
		[| \p -> BS.packCStringLen
			(#{ptr VkExtensionProperties, extensionName} p,
				#{const VK_MAX_EXTENSION_NAME_SIZE}) |],
		[| \p bs -> BS.useAsCStringLen bs \(cs, ln) -> copyBytes (
			#{ptr VkExtensionProperties, extensionName} p) cs ln |]
			),
	("specVersion", ''#{type uint32_t},
		[| #{peek VkExtensionProperties, specVersion} |],
		[| #{poke VkExtensionProperties, specVersion} |]) ]
	[''Show, ''Storable]

foreign import ccall "vkEnumerateInstanceExtensionProperties"
	instanceExtensionProperties ::
	CString -> Ptr #{type uint32_t} -> Ptr ExtensionProperties ->
	IO #{type VkResult}
