{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.SpecializationInfo.Internal where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct
import Data.Word

import Vulkan.Base

#include <vulkan/vulkan.h>

struct "SpecializationMapEntry" #{size VkSpecializationMapEntry}
		#{alignment VkSpecializationMapEntry} [
	("constantId", ''#{type uint32_t},
		[| #{peek VkSpecializationMapEntry, constantID} |],
		[| #{poke VkSpecializationMapEntry, constantID} |]),
	("offset", ''#{type uint32_t},
		[| #{peek VkSpecializationMapEntry, offset} |],
		[| #{poke VkSpecializationMapEntry, offset} |]),
	("size", ''#{type size_t},
		[| #{peek VkSpecializationMapEntry, size} |],
		[| #{poke VkSpecializationMapEntry, size} |]) ]
	[''Show, ''Storable]

type PtrSpecializationMapEntry = Ptr SpecializationMapEntry

struct "SpecializationInfo" #{size VkSpecializationInfo}
		#{alignment VkSpecializationInfo} [
	("mapEntryCount", ''#{type uint32_t},
		[| #{peek VkSpecializationInfo, mapEntryCount} |],
		[| #{poke VkSpecializationInfo, mapEntryCount} |]),
	("pMapEntries", ''PtrSpecializationMapEntry,
		[| #{peek VkSpecializationInfo, pMapEntries} |],
		[| #{poke VkSpecializationInfo, pMapEntries} |]),
	("dataSize", ''#{type size_t},
		[| #{peek VkSpecializationInfo, dataSize} |],
		[| #{poke VkSpecializationInfo, dataSize} |]),
	("pData", ''PtrVoid,
		[| #{peek VkSpecializationInfo, pData} |],
		[| #{poke VkSpecializationInfo, pData} |]) ]
	[''Show]
