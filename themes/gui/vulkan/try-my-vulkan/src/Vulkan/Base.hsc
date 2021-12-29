{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Base where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Data.Word

#include <vulkan/vulkan.h>

type PtrVoid = Ptr ()

type ListCFloat = [#{type float}]

class Pointable a where
	withPointer :: a -> (Ptr a -> IO b) -> IO b
	fromPointer :: Ptr a -> IO a

instance {-# OVERLAPPABLE #-} Storable a => Pointable a where
	withPointer x f = alloca \p -> poke p x >> f p
	fromPointer = peek

pokeCStringLen :: Int -> CString -> String -> IO ()
pokeCStringLen n cs str = withCString str \cs_ -> copyBytes cs cs_ n

pokeCString :: CString -> String -> IO ()
pokeCString cs str = withCStringLen str \(cs_, ln) -> do
	copyBytes cs cs_ ln
	poke (cs `plusPtr` ln :: CString) 0

enum "Bool32" ''#{type VkBool32} [''Show, ''Storable] [
	("VkFalse", #{const VK_FALSE}), ("VkTrue", #{const VK_TRUE}) ]
