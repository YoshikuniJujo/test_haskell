{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Instance.Type (I(..)) where

import qualified Vulkan.Instance.Middle as M

newtype I s = I { unI :: M.I } deriving Show
