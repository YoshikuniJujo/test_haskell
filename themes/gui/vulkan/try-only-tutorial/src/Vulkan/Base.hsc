{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Base where

import Foreign.Ptr
import Foreign.C.String
import Data.Int

#include <vulkan/vulkan.h>

type PtrVoid = Ptr ()

pattern NullPtr :: Ptr a
pattern NullPtr <- ((== nullPtr) -> True) where NullPtr = nullPtr

type PtrCString = Ptr CString

success :: #{type VkResult}
success = #{const VK_SUCCESS}
