{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PushConstant.Middle.Internal where

import Data.Word

import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.PushConstant.Core as C

data Range = Range {
	rangeStageFlags :: ShaderStageFlags,
	rangeOffset :: Word32,
	rangeSize :: Word32 }
	deriving Show

rangeToCore :: Range -> C.Range
rangeToCore Range {
	rangeStageFlags = ShaderStageFlagBits sf,
	rangeOffset = ost, rangeSize = sz
	} = C.Range {
		C.rangeStageFlags = sf,
		C.rangeOffset = ost, C.rangeSize = sz }
