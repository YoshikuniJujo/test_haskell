{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Query (

	-- * TYPE SYNONYMS

	Q, First, Count,

	-- * ENUM

	module Gpu.Vulkan.Query.Enum

	) where

import Data.Word
import Gpu.Vulkan.Query.Enum

type First = Word32
type Count = Word32
type Q = Word32
