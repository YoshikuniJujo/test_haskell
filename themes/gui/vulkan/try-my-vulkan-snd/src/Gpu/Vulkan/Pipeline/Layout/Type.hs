{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout.Type where

import qualified Gpu.Vulkan.Pipeline.Layout.Middle as M

newtype L s = L M.L deriving Show
