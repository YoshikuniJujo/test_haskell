{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.InputAssemblyState.Internal where

import Prelude hiding (Bool(..))

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

import Vulkan.Base
import Vulkan.PrimitiveTopology

import qualified Vulkan.StructureType as ST

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineInputAssemblyStateCreateFlags}
	[''Show, ''Storable] [ ("CreateFlagsZero", 0) ]

struct "CreateInfo" #{size VkPipelineInputAssemblyStateCreateInfo}
		#{alignment VkPipelineInputAssemblyStateCreateInfo} [
	("sType", ''(), [| const $ pure () |],
		[| \p _ -> #{poke VkPipelineInputAssemblyStateCreateInfo, sType}
			p ST.pipelineInputAssemblyStateCreateInfo |]),
	("pNext", ''PtrVoid,
		[| #{peek VkPipelineInputAssemblyStateCreateInfo, pNext} |],
		[| #{poke VkPipelineInputAssemblyStateCreateInfo, pNext} |]),
	("flags", ''CreateFlags,
		[| #{peek VkPipelineInputAssemblyStateCreateInfo, flags} |],
		[| #{poke VkPipelineInputAssemblyStateCreateInfo, flags} |]),
	("topology", ''PrimitiveTopology,
		[| #{peek VkPipelineInputAssemblyStateCreateInfo, topology} |],
		[| #{poke VkPipelineInputAssemblyStateCreateInfo, topology} |]),
	("primitiveRestartEnable", ''Bool32,
		[| #{peek VkPipelineInputAssemblyStateCreateInfo,
			primitiveRestartEnable} |],
		[| #{poke VkPipelineInputAssemblyStateCreateInfo,
			primitiveRestartEnable} |])
	]
	[''Show, ''Storable]

type PtrCreateInfo = Ptr CreateInfo
