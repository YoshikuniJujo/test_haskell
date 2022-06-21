{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Fence where

import qualified Gpu.Vulkan.Fence.Core as C

newtype F = F C.F

fToCore :: F -> C.F

maybeFToCore :: Maybe F -> C.F
