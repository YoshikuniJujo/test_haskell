{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Surface (create, Vk.Khr.Sfc.S) where

import Data.TypeLevel.ParMaybe qualified as TPMaybe

import Graphics.SimplePolygon.Window qualified as Win
import Graphics.SimplePolygon.Instance qualified as Ist

import Gpu.Vulkan.Khr.Surface qualified as Vk.Khr.Sfc
import Gpu.Vulkan.Khr.Surface.Glfw.Window qualified as Vk.Khr.Sfc.Glfw

create :: Ist.I si -> Win.W sw -> (forall ss . Vk.Khr.Sfc.S ss -> IO a) -> IO a
create i (Win.W w _) = Vk.Khr.Sfc.Glfw.create i w TPMaybe.N
