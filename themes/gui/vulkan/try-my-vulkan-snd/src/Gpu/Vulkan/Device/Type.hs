{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Type where

import Gpu.Vulkan.Memory.Core as Memory.C

import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as M

newtype D s = D M.D deriving Show
