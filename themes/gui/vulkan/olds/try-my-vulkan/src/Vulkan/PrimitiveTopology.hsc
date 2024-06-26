-- This file is automatically generated by the tools/makeEnumVkPrimitiveTopology.hs
--	% stack runghc --cwd tools/ makeEnumVkPrimitiveTopology.hs

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PrimitiveTopology where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

#include <vulkan/vulkan.h>

enum "PrimitiveTopology" ''#{type VkPrimitiveTopology} [''Show, ''Eq, ''Storable] [
	("PrimitiveTopologyPointList",
		#{const VK_PRIMITIVE_TOPOLOGY_POINT_LIST}),
	("PrimitiveTopologyLineList", #{const VK_PRIMITIVE_TOPOLOGY_LINE_LIST}),
	("PrimitiveTopologyLineStrip",
		#{const VK_PRIMITIVE_TOPOLOGY_LINE_STRIP}),
	("PrimitiveTopologyTriangleList",
		#{const VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST}),
	("PrimitiveTopologyTriangleStrip",
		#{const VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP}),
	("PrimitiveTopologyTriangleFan",
		#{const VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN}),
	("PrimitiveTopologyLineListWithAdjacency",
		#{const VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY}),
	("PrimitiveTopologyLineStripWithAdjacency",
		#{const VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY}),
	("PrimitiveTopologyTriangleListWithAdjacency",
		#{const VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY}),
	("PrimitiveTopologyTriangleStripWithAdjacency",
		#{const VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY}),
	("PrimitiveTopologyPatchList",
		#{const VK_PRIMITIVE_TOPOLOGY_PATCH_LIST}),
	("PrimitiveTopologyMaxEnum", #{const VK_PRIMITIVE_TOPOLOGY_MAX_ENUM}) ]
