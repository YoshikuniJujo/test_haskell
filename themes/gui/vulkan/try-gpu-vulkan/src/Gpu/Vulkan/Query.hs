{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Query (

	-- * TYPE SYNONYMS

	Q, First, Count

	) where

import Data.Word

type First = Word32
type Count = Word32
type Q = Word32