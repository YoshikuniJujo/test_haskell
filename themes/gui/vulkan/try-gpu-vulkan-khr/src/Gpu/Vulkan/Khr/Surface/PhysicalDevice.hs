{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Khr.Surface.PhysicalDevice (

	-- * GET SUPPORT, FORMATS, CAPABILITIES AND PRESENT MODES

	getSupport, getFormats, getFormatsNew, getFormatsFiltered, getCapabilities, getPresentModes

	) where

import Data.HeteroParList qualified as HeteroParList

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

getFormats :: PhysicalDevice.P -> S ss -> IO [M.Format]
getFormats phdvc (S sfc) = M.getFormats phdvc sfc

getFormatsNew :: PhysicalDevice.P -> S ss ->
	(forall fmts . HeteroParList.ToListWithC T.FormatToValue fmts => HeteroParList.PL FormatNew fmts -> IO a) -> IO a
getFormatsNew pd sfc f = getFormats pd sfc >>= \fmts -> formatListToNew fmts f

getFormatsFiltered :: T.FormatToValue fmt => PhysicalDevice.P -> S ss -> IO [FormatNew fmt]
getFormatsFiltered pd sfc = formatFilter <$> getFormats pd sfc

getPresentModes :: PhysicalDevice.P -> S ss -> IO [PresentMode]
getPresentModes phdvc (S sfc) = M.getPresentModes phdvc sfc
