{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.PhysicalDevice (

	-- * GET SUPPORT, FORMATS, CAPABILITIES AND PRESENT MODES

	getSupport, getFormatsOld, getFormats, getFormatsFiltered, getCapabilities, getPresentModes

	) where

import Data.HeteroParList.Constrained qualified as HeteroParListC

import Gpu.Vulkan.TypeEnum qualified as T
import Gpu.Vulkan.Khr.Enum
import Gpu.Vulkan.Khr.Surface.Type
import Gpu.Vulkan.Khr.Surface.Internal

import qualified Gpu.Vulkan.PhysicalDevice as PhysicalDevice
import qualified Gpu.Vulkan.QueueFamily as QueueFamily
import qualified Gpu.Vulkan.Khr.Surface.Middle.Internal as M
import qualified Gpu.Vulkan.Khr.Surface.PhysicalDevice.Middle as M

getSupport :: PhysicalDevice.P -> QueueFamily.Index -> S ss -> IO Bool
getSupport phdvc qfi (S sfc) = M.getSupport phdvc qfi sfc

getCapabilities :: PhysicalDevice.P -> S ss -> IO M.Capabilities
getCapabilities phdvc (S sfc) = M.getCapabilities phdvc sfc

getFormatsOld :: PhysicalDevice.P -> S ss -> IO [M.Format]
getFormatsOld phdvc (S sfc) = M.getFormats phdvc sfc

getFormats :: PhysicalDevice.P -> S ss ->
	(forall fmts .
		Show (HeteroParListC.PL T.FormatToValue Format fmts) =>
		HeteroParListC.PL T.FormatToValue Format fmts -> IO a) -> IO a
getFormats pd sfc f = getFormatsOld pd sfc >>= \fmts -> formatListToNew fmts f

getFormatsFiltered :: T.FormatToValue fmt => PhysicalDevice.P -> S ss -> IO [Format fmt]
getFormatsFiltered pd sfc = formatFilter <$> getFormatsOld pd sfc

getPresentModes :: PhysicalDevice.P -> S ss -> IO [PresentMode]
getPresentModes phdvc (S sfc) = M.getPresentModes phdvc sfc
