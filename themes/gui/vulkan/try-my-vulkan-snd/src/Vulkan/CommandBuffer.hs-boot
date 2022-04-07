{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandBuffer where

import qualified Vulkan.CommandBuffer.Core as C

newtype C = C C.C

instance Show C
