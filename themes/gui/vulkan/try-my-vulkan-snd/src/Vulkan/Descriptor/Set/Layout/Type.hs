{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Set.Layout.Type where

import qualified Vulkan.Descriptor.Set.Layout.Middle as M

newtype L s = L { unL :: M.L } deriving Show
