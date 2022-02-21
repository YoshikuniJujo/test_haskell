{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Enumerate where

import Vulkan

import qualified Data.ByteString as BS

import qualified Vulkan.Enumerate.Core as C

data LayerProperties = LayerProperties {
	layerPropertiesLayerName :: String,
	layerPropertiesSpecVersion :: ApiVersion,
	layerPropertiesImplementationVersion :: ApiVersion,
	description :: String }
	deriving Show
