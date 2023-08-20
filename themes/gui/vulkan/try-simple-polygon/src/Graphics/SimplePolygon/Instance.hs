{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Instance where

import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Bits
import Graphics.UI.GLFW qualified as Glfw
import Gpu.Vulkan.Misc
import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Instance qualified as Vk.Ist

create :: Bool -> (forall s . Vk.Ist.I s -> IO a) -> IO a
create vld f = do
	gexs <- (cstrToText <$>) <$> Glfw.getRequiredInstanceExtensions
	Vk.Ist.create Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = TMaybe.N,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo =
			Nothing :: Maybe (Vk.ApplicationInfo 'Nothing),
		Vk.Ist.createInfoEnabledLayerNames = [],
		Vk.Ist.createInfoEnabledExtensionNames = [] } TPMaybe.N f
