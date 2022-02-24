{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.DebugUtils.Messenger where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word
import Data.Int

import qualified Data.Text as T

import Vulkan.Ext.DebugUtils

import qualified Vulkan.Ext.DebugUtils.Messenger.Core as C

#include <vulkan/vulkan.h>

enum "CallbackDataFlags" ''#{type VkDebugUtilsMessengerCallbackDataFlagsEXT}
		[''Show, ''Storable] [("CallbackDataFlagsZero", 0)]

data CallbackData n n2 n3 n4 = CallbackData {
	callbackDataNext :: Maybe n,
	callbackDataFlags :: CallbackDataFlags,
	callbackDataMessageIdName :: T.Text,
	callbackDataMessageIdNumber :: Int32,
	callbackDataMessage :: T.Text,
	callbackDataQueueLabels :: [Label n2],
	callbackDataCmdBufLabels :: [Label n3],
	callbackDataObjects :: [ObjectNameInfo n4]
	}
	deriving Show

callbackDataFromCore :: C.CallbackData -> IO (CallbackData n n2 n3 n4)
callbackDataFromCore C.CallbackData {
	} = do
	undefined

-- type FnCallback =
