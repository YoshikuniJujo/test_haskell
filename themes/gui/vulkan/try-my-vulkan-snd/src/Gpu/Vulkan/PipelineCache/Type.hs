{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineCache.Type where

import qualified Gpu.Vulkan.PipelineCache.Middle as M

newtype C s = C M.C deriving Show

cToMiddle :: C s -> M.C
cToMiddle (C c) = c
