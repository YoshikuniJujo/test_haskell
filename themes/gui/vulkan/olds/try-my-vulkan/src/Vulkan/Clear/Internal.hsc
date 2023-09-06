{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Clear.Internal where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Struct
import Data.Int
import Data.Word

#include <vulkan/vulkan.h>

type ListFloat = [#{type float}]

struct "ColorValueFloat" #{size VkClearColorValue}
		#{alignment VkClearColorValue} [
	("rgba", ''ListFloat, [| peekArray 4 . castPtr |],
		[| \p vs -> pokeArray (castPtr p) vs |]) ]
	[''Show, ''Storable]

type ListInt32T = [#{type int32_t}]

struct "ColorValueInt32T" #{size VkClearColorValue}
		#{alignment VkClearColorValue} [
	("rgba", ''ListInt32T, [| peekArray 4 . castPtr |],
		[| \p vs -> pokeArray (castPtr p) vs |]) ]
	[''Show, ''Storable]

type ListUint32T = [#{type uint32_t}]

struct "ColorValueUint32T" #{size VkClearColorValue}
		#{alignment VkClearColorValue} [
	("rgba", ''ListUint32T, [| peekArray 4 . castPtr |],
		[| \p vs -> pokeArray (castPtr p) vs |]) ]
	[''Show, ''Storable]

data ColorValueTag
newtype ColorValue = ColorValue (ForeignPtr ColorValueTag) deriving Show

fromColorValueFloat :: ColorValueFloat -> ColorValue
fromColorValueFloat (ColorValueFloat_ f) = ColorValue $ castForeignPtr f

fromColorValueInt32T :: ColorValueInt32T -> ColorValue
fromColorValueInt32T (ColorValueInt32T_ f) = ColorValue $ castForeignPtr f

fromColorValueUint32T :: ColorValueUint32T -> ColorValue
fromColorValueUint32T (ColorValueUint32T_ f) = ColorValue $ castForeignPtr f
