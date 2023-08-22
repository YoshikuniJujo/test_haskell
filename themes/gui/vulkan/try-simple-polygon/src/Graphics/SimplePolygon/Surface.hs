{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Surface (create) where

import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Graphics.UI.GLFW qualified as Glfw
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.Glfw qualified as Vk.Khr.Sfc.Glfw

import Graphics.SimplePolygon.Instance qualified as Ist

create ::
	Ist.I si -> Glfw.Window -> (forall ss . Vk.Khr.Sfc.S ss -> IO a) -> IO a
create i w = Vk.Khr.Sfc.Glfw.createWindowSurface i w TPMaybe.N
