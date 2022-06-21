{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.CommandBuffer.Middle where

import qualified Gpu.Vulkan.CommandBuffer.Core as C

newtype C vs = C { unC :: C.C }

instance Show (C vs)
