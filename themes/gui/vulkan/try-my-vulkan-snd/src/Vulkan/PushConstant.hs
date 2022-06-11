{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.PushConstant where

import Data.Word

import qualified Vulkan.Shader.Stage.Enum as Shader.Stage
import qualified Vulkan.PushConstant.Core as C

data Range = Range {
	rangeStageFlags :: Shader.Stage.Flags,
	rangeOffset :: Word32,
	rangeSize :: Word32 }
	deriving Show

rangeToCore :: Range -> C.Range
rangeToCore Range {
	rangeStageFlags = Shader.Stage.FlagBits sf,
	rangeOffset = ost, rangeSize = sz
	} = C.Range {
		C.rangeStageFlags = sf,
		C.rangeOffset = ost, C.rangeSize = sz }
