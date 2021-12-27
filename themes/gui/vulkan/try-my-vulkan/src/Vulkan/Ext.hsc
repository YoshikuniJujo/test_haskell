{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext where

import Foreign.C.Types
import Data.Int

import qualified Vulkan.Ext.Internal as I

#include <vulkan/vulkan.h>

debugUtilsExtensionName :: String
debugUtilsExtensionName = #{const_str VK_EXT_DEBUG_UTILS_EXTENSION_NAME}

data DebugUtilsLabel n = DebugUtilsLabel {
	debugUtilsLabelNext :: Maybe n,
	debugUtilsLabelName :: String,
	debugUtilsLabelColor :: (CFloat, CFloat, CFloat, CFloat) }
	deriving Show

data DebugUtilsObjectNameInfo = DebugUtilsObjectNameInfo {
	}
	deriving Show

data DebugUtilsMessengerCallbackData n n' n'' ud = DebugUtilsMessengerCallbackData {
	debugUtilsMessengerCallbackDataNext :: Maybe n,
	debugUtilsMessengerCallbackDataFlags ::
		I.DebugUtilsMessengerCallbackDataFlags,
	debugUtilsMessengerCallbackDataMessageIdName :: String,
	debugUtilsMessengerCallbackDataMessageIdNumber :: #{type int32_t},
	debugUtilsMessengerCallbackDataMessage :: String,
	debugUtilsMessengerCallbackDataQueueLabels :: [DebugUtilsLabel n'],
	debugUtilsMessengerCallbackDataCmdBufferLabels :: [DebugUtilsLabel n'']
--	debugUtilsMessengerCallbackDataObjects :: [I.DebugUtilsObjectNameInfo]
	}
	deriving Show
