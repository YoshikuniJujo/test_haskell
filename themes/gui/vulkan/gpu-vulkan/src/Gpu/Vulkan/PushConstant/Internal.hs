{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PushConstant.Internal (

	-- * PUSHCONSTANT LAYOUT

	Layout(..), Range(..),

	-- * PUSH CONSTANT LAYOUT TO RANGES

	RangeListToMiddle(..)

	) where

import Foreign.Storable.HeteroList
import Data.Kind

import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.PushConstant.Middle qualified as M

data Layout = Layout [Type] [Range]

data Range = Range [T.ShaderStageFlagBits] [Type]

class RangeListToMiddle (whole :: [Type]) (ranges :: [Range]) where
	rangeListToMiddle :: [M.Range]

instance RangeListToMiddle whole '[] where rangeListToMiddle = []

instance (RangeToMiddle whole range, RangeListToMiddle whole ranges) =>
	RangeListToMiddle whole (range ': ranges) where
	rangeListToMiddle =
		rangeToMiddle @whole @range : rangeListToMiddle @whole @ranges

class RangeToMiddle (whole :: [Type]) (range :: Range) where
	rangeToMiddle :: M.Range

instance (T.ShaderStageFlagBitsListToValue sss, InfixOffsetSize part whole) =>
	RangeToMiddle whole ('Range sss part) where
	rangeToMiddle = M.Range {
		M.rangeStageFlags = T.shaderStageFlagBitsListToValue @sss,
		M.rangeOffset = fromIntegral offt,
		M.rangeSize = fromIntegral sz }
		where
		(offt, sz) = infixOffsetSize @part @whole
