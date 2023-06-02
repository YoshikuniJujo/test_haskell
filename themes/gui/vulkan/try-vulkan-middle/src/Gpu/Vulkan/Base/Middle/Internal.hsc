{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Base.Middle.Internal (
	boolToBool32, bool32ToBool, ObjectHandle(..) ) where

import Data.Word

#include <vulkan/vulkan.h>

boolToBool32 :: Bool -> #{type VkBool32}
boolToBool32 False = #{const VK_FALSE}
boolToBool32 True = #{const VK_TRUE}

bool32ToBool :: #{type VkBool32} -> Bool
bool32ToBool #{const VK_FALSE} = False
bool32ToBool #{const VK_TRUE} = True
bool32ToBool _ = error $
	"Application must not pass any other values than " ++
	"VK_TRUE or VK_FALSE into a Gpu.Vulkan implementation " ++
	"where a VkBool32 is expected"

newtype ObjectHandle = ObjectHandle #{type uint64_t} deriving Show
