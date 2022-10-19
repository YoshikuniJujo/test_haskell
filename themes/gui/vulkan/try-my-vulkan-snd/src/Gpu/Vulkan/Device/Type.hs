{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Type where

import Gpu.Vulkan.Memory.Core as Memory.C

import {-# SOURCE #-} qualified Gpu.Vulkan.Device.Middle.Internal as M

newtype D s = D M.D deriving Show

data MemoryImage s = MemoryImage M.Size Memory.C.M deriving Show

newtype MemoryList s v = MemoryList (M.MemoryList v) deriving Show
