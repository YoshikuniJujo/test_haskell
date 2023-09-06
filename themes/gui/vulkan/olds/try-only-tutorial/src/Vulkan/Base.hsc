{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Base where

import Foreign.Ptr
import Foreign.C.String
import Data.Word
import Data.Int

#include <vulkan/vulkan.h>

type PtrVoid = Ptr ()

pattern NullPtr :: Ptr a
pattern NullPtr <- ((== nullPtr) -> True) where NullPtr = nullPtr

pattern NullFunPtr :: FunPtr a
pattern NullFunPtr <- ((== nullFunPtr) -> True) where NullFunPtr = nullFunPtr

type PtrCString = Ptr CString

success :: #{type VkResult}
success = #{const VK_SUCCESS}

type PtrResult = Ptr #{type VkResult}

vkFalse, vkTrue :: #{type VkBool32}
vkFalse = #{const VK_FALSE}
vkTrue = #{const VK_TRUE}

pattern NullHandle :: Ptr a
pattern NullHandle <- (ptrToWordPtr -> (WordPtr #{const VK_NULL_HANDLE})) where
	NullHandle = wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}

queueGraphicsBit :: #{type VkQueueFlags}
queueGraphicsBit = #{const VK_QUEUE_GRAPHICS_BIT}

type PtrFloat = Ptr #{type float}

uint32Max :: #{type uint32_t}
uint32Max = #{const UINT32_MAX}

type PtrUint32T = Ptr #{type uint32_t}

data FramebufferTag
type Framebuffer = Ptr FramebufferTag

uint64Max :: #{type uint64_t}
uint64Max = #{const UINT64_MAX}
