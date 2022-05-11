{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Fence where

import qualified Vulkan.Fence.Core as C

newtype F = F C.F

fToCore :: F -> C.F

maybeFToCore :: Maybe F -> C.F
