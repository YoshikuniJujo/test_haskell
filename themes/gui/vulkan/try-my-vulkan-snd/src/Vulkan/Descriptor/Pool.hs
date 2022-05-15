{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Pool where

import Data.Word

import Vulkan.Descriptor.Enum

import qualified Vulkan.Descriptor.Pool.Core as C

data Size = Size { sizeType :: Type, sizeDescriptorCount :: Word32 }
	deriving Show

sizeToCore :: Size -> C.Size
sizeToCore Size { sizeType = Type tp, sizeDescriptorCount = dc } =
	C.Size { C.sizeType = tp, C.sizeDescriptorCount = dc }
