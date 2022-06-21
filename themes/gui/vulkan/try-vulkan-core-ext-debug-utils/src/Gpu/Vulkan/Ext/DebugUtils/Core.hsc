{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Core where

import Foreign.Ptr
import Foreign.Ptr.Synonyms
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Control.Monad
import Data.Word

#include <vulkan/vulkan.h>

sTypeL, sTypeO :: #{type VkStructureType}
sTypeL = #{const VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT}
sTypeO = #{const VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT}

struct "Label" #{size VkDebugUtilsLabelEXT} #{alignment VkDebugUtilsLabelEXT} [
	("sType", ''(), [| sTypeCheck sTypeL |],
		[| \p _ -> #{poke VkDebugUtilsLabelEXT, sType} p sTypeL |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDebugUtilsLabelEXT, pNext} |],
		[| #{poke VkDebugUtilsLabelEXT, pNext} |]),
	("pLabelName", ''CString,
		[| #{peek VkDebugUtilsLabelEXT, pLabelName} |],
		[| #{poke VkDebugUtilsLabelEXT, pLabelName} |]),
	("color", ''ListFloat,
		[| peekArray 4 . #{ptr VkDebugUtilsLabelEXT, color} |],
		[| \p c -> pokeArray (#{ptr VkDebugUtilsLabelEXT, color} p)
			$ take 4 c |]) ]
	[''Show, ''Storable]

type PtrLabel = Ptr Label

sTypeCheck :: #{type VkStructureType} -> Ptr a -> IO ()
sTypeCheck st p = do
	st' <- peek $ castPtr p
	when (st /= st') $ error "Vulkan Structure Type not match"

struct "ObjectNameInfo" #{size VkDebugUtilsObjectNameInfoEXT}
		#{alignment VkDebugUtilsObjectNameInfoEXT} [
	("sType", ''(), [| sTypeCheck sTypeO |],
		[| \p _ -> #{poke VkDebugUtilsObjectNameInfoEXT, sType}
			p sTypeO |]),
	("pNext", ''PtrVoid,
		[| #{peek VkDebugUtilsObjectNameInfoEXT, pNext} |],
		[| #{poke VkDebugUtilsObjectNameInfoEXT, pNext} |]),
	("objectType", ''#{type VkObjectType},
		[| #{peek VkDebugUtilsObjectNameInfoEXT, objectType} |],
		[| #{poke VkDebugUtilsObjectNameInfoEXT, objectType} |]),
	("objectHandle", ''#{type uint64_t},
		[| #{peek VkDebugUtilsObjectNameInfoEXT, objectHandle} |],
		[| #{poke VkDebugUtilsObjectNameInfoEXT, objectHandle} |]),
	("pObjectName", ''CString,
		[| #{peek VkDebugUtilsObjectNameInfoEXT, pObjectName} |],
		[| #{poke VkDebugUtilsObjectNameInfoEXT, pObjectName} |]) ]
	[''Show, ''Storable]

type PtrObjectNameInfo = Ptr ObjectNameInfo
