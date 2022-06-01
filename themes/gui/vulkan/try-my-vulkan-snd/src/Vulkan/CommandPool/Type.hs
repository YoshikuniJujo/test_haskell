{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.CommandPool.Type where

import qualified Vulkan.CommandPool.Middle as M

newtype C s = C M.C deriving Show
