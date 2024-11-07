{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Type where

import qualified Gpu.Vulkan.Device.Middle as M

newtype D s = D M.D deriving Show
