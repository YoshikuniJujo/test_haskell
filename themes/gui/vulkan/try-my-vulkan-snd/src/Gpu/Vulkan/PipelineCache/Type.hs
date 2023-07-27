{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache.Type (

	-- * PIPELINE CACHE

	P(..), pToMiddle

	) where

import qualified Gpu.Vulkan.PipelineCache.Middle as M

newtype P s = P M.C deriving Show

pToMiddle :: P s -> M.C
pToMiddle (P c) = c
