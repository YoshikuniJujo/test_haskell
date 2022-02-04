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

vkFalse :: #{type VkBool32}
vkFalse = #{const VK_FALSE}

pattern NullHandle :: Ptr a
pattern NullHandle <- (ptrToWordPtr -> (WordPtr #{const VK_NULL_HANDLE})) where
	NullHandle = wordPtrToPtr $ WordPtr #{const VK_NULL_HANDLE}

queueGraphicsBit :: #{type VkQueueFlags}
queueGraphicsBit = #{const VK_QUEUE_GRAPHICS_BIT}
