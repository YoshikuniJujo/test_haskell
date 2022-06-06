{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Layout.Type where

import qualified Vulkan.Pipeline.Layout.Middle as M

newtype L s = L M.L deriving Show
