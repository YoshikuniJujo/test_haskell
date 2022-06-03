{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device.Type where

import qualified Vulkan.Device.Middle as M

newtype D s = D M.D deriving Show

newtype MemoryImage s = MemoryImage M.MemoryImage deriving Show
