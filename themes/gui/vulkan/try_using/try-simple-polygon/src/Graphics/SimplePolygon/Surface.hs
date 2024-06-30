{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Surface (create, Vk.Khr.Sfc.S) where

import Data.TypeLevel.ParMaybe qualified as TPMaybe

import Graphics.SimplePolygon.Instance qualified as Ist

import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw

import Graphics.UI.GlfwG.Window qualified as GlfwG.Win

create :: Ist.I si -> GlfwG.Win.W sw -> (forall ss . Vk.Khr.Sfc.S ss -> IO a) -> IO a
create i w = Vk.Khr.Sfc.Glfw.create i w TPMaybe.N
