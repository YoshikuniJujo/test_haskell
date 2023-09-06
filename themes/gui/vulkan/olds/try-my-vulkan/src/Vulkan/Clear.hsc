{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Clear where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.C.Struct
import Control.Monad.Cont
import Data.Foldable
import Data.List.Length
import Data.Word
import Data.Int

import Vulkan.Base

import qualified Vulkan.Clear.Internal as I

#include <vulkan/vulkan.h>

data ColorValueFloat =
	ColorValueFloat { colorValueFloatRgba :: LengthL 4 Float }
	deriving Show

fromColorValueFloat :: ColorValueFloat -> I.ColorValue
fromColorValueFloat = I.fromColorValueFloat . colorValueFloatToC

colorValueFloatToC :: ColorValueFloat -> I.ColorValueFloat
colorValueFloatToC (ColorValueFloat rgba) =
	I.ColorValueFloat $ floatToFloat <$> toList rgba

data ColorValueInt32 =
	ColorValueInt32 { colorValueInt32Rgba :: LengthL 4 Int32 }
	deriving Show

fromColorValueInt32 :: ColorValueInt32 -> I.ColorValue
fromColorValueInt32 = I.fromColorValueInt32T . colorValueInt32ToC

colorValueInt32ToC :: ColorValueInt32 -> I.ColorValueInt32T
colorValueInt32ToC (ColorValueInt32 rgba) =
	I.ColorValueInt32T $ int32ToInt32T <$> toList rgba

data ColorValueWord32 =
	ColorValueWord32 { colorValueWord32Rgba :: LengthL 4 Word32 }

fromColorValueWord32 :: ColorValueWord32 -> I.ColorValue
fromColorValueWord32 = I.fromColorValueUint32T . colorValueWord32ToC

colorValueWord32ToC :: ColorValueWord32 -> I.ColorValueUint32T
colorValueWord32ToC (ColorValueWord32 rgba) =
	I.ColorValueUint32T $ word32ToUint32T <$> toList rgba

struct "DepthStencilValue" #{size VkClearDepthStencilValue}
		#{alignment VkClearDepthStencilValue} [
	("depth", ''#{type float},
		[| #{peek VkClearDepthStencilValue, depth} |],
		[| #{poke VkClearDepthStencilValue, depth} |]),
	("stencil", ''#{type uint32_t},
		[| #{peek VkClearDepthStencilValue, stencil} |],
		[| #{poke VkClearDepthStencilValue, stencil} |]) ]
	[''Show, ''Storable]

data ValueTag
newtype Value = Value (ForeignPtr ValueTag) deriving Show

fromColorValue :: I.ColorValue -> Value
fromColorValue (I.ColorValue f) = Value $ castForeignPtr f

fromDepthStencilValue :: DepthStencilValue -> Value
fromDepthStencilValue (DepthStencilValue_ f) = Value $ castForeignPtr f

type ValuePtr = Ptr ValueTag

valueToPtr :: Value -> ContT r IO ValuePtr
valueToPtr (Value f) = ContT $ withForeignPtr f

type PtrValue = Ptr ValuePtr
