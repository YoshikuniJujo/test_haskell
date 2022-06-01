{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer.Middle where

import qualified Vulkan.CommandBuffer.Core as C

newtype C vs = C { unC :: C.C }

instance Show (C vs)
