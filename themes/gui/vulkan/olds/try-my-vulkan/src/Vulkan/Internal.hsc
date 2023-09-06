{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Internal where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Data.List
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
	Word8 -> Word8 -> Word16 -> Word16 -> ApiVersion

apiVersion1_0 :: ApiVersion
apiVersion1_0 = makeApiVersion 0 1 0 0

enum "InstanceCreateFlags" ''#{type VkInstanceCreateFlags}
	[''Show, ''Read, ''Eq, ''Storable] [("InstanceCreateFlagsZero", 0)]

type PtrApplicationInfo = Ptr ApplicationInfo

struct "InstanceCreateInfo" #{size VkInstanceCreateInfo}
		#{alignment VkInstanceCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkInstanceCreateInfo, sType} p
			(#{const VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO} ::
				#{type VkStructureType}) |]),
	("pNext", ''PtrVoid, [| #{peek VkInstanceCreateInfo, pNext} |],
		[| #{poke VkInstanceCreateInfo, pNext} |]),
	("flags", ''InstanceCreateFlags,
		[| #{peek VkInstanceCreateInfo, flags} |],
		[| #{poke VkInstanceCreateInfo, flags} |]),
	("pApplicationInfo", ''PtrApplicationInfo,
		[| #{peek VkInstanceCreateInfo, pApplicationInfo} |],
		[| #{poke VkInstanceCreateInfo, pApplicationInfo} |]),
	("enabledLayerCount", ''#{type uint32_t},
		[| #{peek VkInstanceCreateInfo, enabledLayerCount} |],
		[| #{poke VkInstanceCreateInfo, enabledLayerCount} |]),
	("ppEnabledLayerNames", ''PtrCString,
		[| #{peek VkInstanceCreateInfo, ppEnabledLayerNames} |],
		[| #{poke VkInstanceCreateInfo, ppEnabledLayerNames} |]),
	("enabledExtensionCount", ''#{type uint32_t},
		[| #{peek VkInstanceCreateInfo, enabledExtensionCount} |],
		[| #{poke VkInstanceCreateInfo, enabledExtensionCount} |]),
	("ppEnabledExtensionNames", ''PtrCString,
		[| #{peek VkInstanceCreateInfo, ppEnabledExtensionNames} |],
		[| #{poke VkInstanceCreateInfo, ppEnabledExtensionNames} |]) ]
	[''Show]

withCStrings :: Num n => [String] -> (n -> Ptr CString -> IO a) -> IO a
withCStrings strs f = do
	cstrs <- newCString `mapM` strs
	withArray cstrs (f $ genericLength strs) <* free `mapM_` cstrs
