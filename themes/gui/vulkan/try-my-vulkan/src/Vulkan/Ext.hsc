{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext where

import Foreign.C.Types
import Data.Word
import Data.Int

import Vulkan

import qualified Vulkan.Ext.Internal as I

#include <vulkan/vulkan.h>

debugUtilsExtensionName :: String
debugUtilsExtensionName = #{const_str VK_EXT_DEBUG_UTILS_EXTENSION_NAME}

data DebugUtilsLabel n = DebugUtilsLabel {
	debugUtilsLabelNext :: Maybe n,
	debugUtilsLabelName :: String,
	debugUtilsLabelColor :: (CFloat, CFloat, CFloat, CFloat) }
	deriving Show

data DebugUtilsObjectNameInfo n = DebugUtilsObjectNameInfo {
	debugUtilsObjectNameInfoNext :: Maybe n,
	debugUtilsObjectNemeInfoObjectType :: ObjectType,
	debugUtilsObjectNameInfoObjectHandle :: #{type uint64_t},
	debugUtilsObjectNameInfoObjectName :: String }
	deriving Show

data DebugUtilsMessengerCallbackData n n1 n2 n3 =
	DebugUtilsMessengerCallbackData {
		debugUtilsMessengerCallbackDataNext :: Maybe n,
		debugUtilsMessengerCallbackDataFlags ::
			I.DebugUtilsMessengerCallbackDataFlags,
		debugUtilsMessengerCallbackDataMessageIdName :: String,
		debugUtilsMessengerCallbackDataMessageIdNumber ::
			#{type int32_t},
		debugUtilsMessengerCallbackDataMessage :: String,
		debugUtilsMessengerCallbackDataQueueLabels ::
			[DebugUtilsLabel n1],
		debugUtilsMessengerCallbackDataCmdBufferLabels ::
			[DebugUtilsLabel n2],
		debugUtilsMessengerCallbackDataObjects ::
			[DebugUtilsObjectNameInfo n3] }
	deriving Show

type FnDebugUtilsMessengerCallback n n1 n2 n3 ud =
	I.DebugUtilsMessageSeverityFlagBits ->
	I.DebugUtilsMessageTypeFlagBits ->
	DebugUtilsMessengerCallbackData n n1 n2 n3 -> Maybe ud -> IO ()

fnDebugUtilsMessengerCallbackToC ::
	FnDebugUtilsMessengerCallback n n1 n2 n3 ud ->
	I.FnDebugUtilsMessengerCallback
fnDebugUtilsMessengerCallbackToC f s t dt ud = #{const VK_FALSE} <$ f s t undefined undefined
