{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.DebugUtils.Core where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

sTypeL :: #{type VkStructureType}
sTypeL = #{const VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT}

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
