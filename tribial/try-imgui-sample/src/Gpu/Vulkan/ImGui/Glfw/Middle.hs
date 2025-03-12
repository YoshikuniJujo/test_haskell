{-# LANGUAGE ImportQualifiedPost #-}

module Gpu.Vulkan.ImGui.Glfw.Middle (init) where

import Prelude hiding (init)
import Data.Bool

import Graphics.UI.GlfwG.Window.Type qualified as GlfwG.Win

import Gpu.Vulkan.ImGui.Glfw.Core qualified as C

init :: GlfwG.Win.W sw -> Bool -> IO Bool
init (GlfwG.Win.W win) ics = (/= 0) <$> C.init win (bool 0 1 ics)
