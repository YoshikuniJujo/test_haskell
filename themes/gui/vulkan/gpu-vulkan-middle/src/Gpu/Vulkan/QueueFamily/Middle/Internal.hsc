{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.QueueFamily.Middle.Internal where

import Data.Word

import Gpu.Vulkan.Core

import qualified Gpu.Vulkan.Queue.Enum as Queue
import qualified Gpu.Vulkan.QueueFamily.Core as C

data Properties = Properties {
	propertiesQueueFlags :: Queue.Flags,
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
	propertiesQueueFlags = Queue.FlagBits flgs,
	propertiesQueueCount = cnt,
	propertiesTimestampValidBits = tvb,
	propertiesMinImageTransferGranularity = mitg }
