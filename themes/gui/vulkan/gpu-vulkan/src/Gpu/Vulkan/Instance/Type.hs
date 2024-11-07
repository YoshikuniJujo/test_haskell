{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Instance.Type (I(..)) where

import qualified Gpu.Vulkan.Instance.Middle as M

newtype I s = I { unI :: M.I } deriving Show
