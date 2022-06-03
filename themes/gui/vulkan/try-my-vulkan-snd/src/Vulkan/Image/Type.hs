{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Image.Type where

import qualified Vulkan.Image.Middle as M

newtype I s = I M.I deriving Show
