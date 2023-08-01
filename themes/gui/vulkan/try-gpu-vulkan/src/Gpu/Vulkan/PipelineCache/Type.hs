{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache.Type (

	-- * PIPELINE CACHE

	P(..)

	) where

import qualified Gpu.Vulkan.PipelineCache.Middle as M

newtype P s = P { pToMiddle :: M.P } deriving Show
