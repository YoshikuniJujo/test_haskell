{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PushConstant where

import Data.Word

import Vulkan.Enum

import qualified Vulkan.PushConstant.Core as C

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
