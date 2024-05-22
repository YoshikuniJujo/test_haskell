{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Surface (create, Vk.Khr.Sfc.S) where

import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.Glfw qualified as Vk.Khr.Sfc.Glfw

import Graphics.SimplePolygon.Window qualified as Win
import Graphics.SimplePolygon.Instance qualified as Ist

create :: Ist.I si -> Win.W -> (forall ss . Vk.Khr.Sfc.S ss -> IO a) -> IO a
create i (Win.W w _) = Vk.Khr.Sfc.Glfw.createWindowSurface i w TPMaybe.N
