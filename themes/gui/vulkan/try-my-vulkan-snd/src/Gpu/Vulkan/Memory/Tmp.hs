{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Memory.Tmp where

import Data.Word

newtype TypeIndex = TypeIndex Word32
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral)
