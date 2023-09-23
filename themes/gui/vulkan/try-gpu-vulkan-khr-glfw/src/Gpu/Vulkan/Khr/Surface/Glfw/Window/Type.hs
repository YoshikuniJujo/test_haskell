{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.Glfw.Window.Type where

import Graphics.UI.GLFW qualified as B

newtype W s = W B.Window deriving Show
