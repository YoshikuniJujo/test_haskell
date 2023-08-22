{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.SimplePolygon.Instance (create, Vk.Ist.I) where

import Foreign.Storable.PeekPoke
import Control.Monad
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Bits
import Data.Bool
import Graphics.UI.GLFW qualified as Glfw
import Gpu.Vulkan.Misc
import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.Instance qualified as Vk.Ist

import Graphics.SimplePolygon.DebugMessenger qualified as DbgMsngr

create :: Bool -> (forall s . Vk.Ist.I s -> IO a) -> IO a
create vld = createGen vld \nxt -> \f' -> do
	when vld $ bool
		(error "validation layers requested, but not available!")
		(pure ()) =<< DbgMsngr.checkLayer
	gexs <- (Vk.ExtensionName <$>)
		<$> ((cstrToText `mapM`) =<< Glfw.getRequiredInstanceExtensions)
	let	exs = bool id (DbgMsngr.extensionName :) vld gexs
	Vk.Ist.create Vk.Ist.CreateInfo {
		Vk.Ist.createInfoNext = nxt,
		Vk.Ist.createInfoFlags = zeroBits,
		Vk.Ist.createInfoApplicationInfo =
			Nothing :: Maybe (Vk.ApplicationInfo 'Nothing),
		Vk.Ist.createInfoEnabledLayerNames =
			bool [] DbgMsngr.validationLayers vld,
		Vk.Ist.createInfoEnabledExtensionNames = exs } TPMaybe.N f'

createGen :: Bool ->
	(forall mn . WithPoked (TMaybe.M mn) => TMaybe.M mn -> (forall s . Vk.Ist.I s -> IO a) -> IO a) ->
	(forall s . Vk.Ist.I s -> IO a) -> IO a
createGen False f = f TMaybe.N
createGen True f = f $ TMaybe.J DbgMsngr.createInfo
