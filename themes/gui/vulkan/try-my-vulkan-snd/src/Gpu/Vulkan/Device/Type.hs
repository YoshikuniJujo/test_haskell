{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Type where

import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle as M

newtype D s = D M.D deriving Show

newtype MemoryImage s = MemoryImage M.MemoryImage deriving Show

newtype MemoryList s v = MemoryList (M.MemoryList v) deriving Show
