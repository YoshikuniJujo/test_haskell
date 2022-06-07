{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Framebuffer.Type where

import qualified Vulkan.Framebuffer.Middle as M

newtype F s = F M.F deriving Show
