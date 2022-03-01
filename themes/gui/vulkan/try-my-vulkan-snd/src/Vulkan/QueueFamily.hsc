{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.QueueFamily where

import Data.Word

import Vulkan.Core
import Vulkan.Enum

import qualified Vulkan.QueueFamily.Core as C

data Properties = Properties {
	propertiesQueueFlags :: QueueFlags,
	propertiesQueueCount :: Word32,
	propertiesTimestampValidBits :: Word32,
	propertiesMinImageTransferGranularity :: Extent3d }
	deriving Show

propertiesFromCore :: C.Properties -> Properties
propertiesFromCore C.Properties {
	C.propertiesQueueFlags = flgs,
	C.propertiesQueueCount = cnt,
	C.propertiesTimestampValidBits = tvb,
	C.propertiesMinImageTransferGranularity = mitg } = Properties {
	propertiesQueueFlags = QueueFlagBits flgs,
	propertiesQueueCount = cnt,
	propertiesTimestampValidBits = tvb,
	propertiesMinImageTransferGranularity = mitg }
