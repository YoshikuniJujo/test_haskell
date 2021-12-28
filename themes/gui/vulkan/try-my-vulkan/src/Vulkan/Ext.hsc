{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
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
	debugUtilsLabelColor :: Maybe (CFloat, CFloat, CFloat, CFloat) }
	deriving Show

debugUtilsLabelFromC :: Pointable n => I.DebugUtilsLabel -> IO (DebugUtilsLabel n)
debugUtilsLabelFromC I.DebugUtilsLabel {
	I.debugUtilsLabelPNext = pnxt, I.debugUtilsLabelPLabelName = cnm,
	I.debugUtilsLabelColor = clr } = do
	mnxt <- fromPointerMaybe $ castPtr pnxt
	nm <- peekCString cnm
	let	mclr = case clr of
			[0, 0, 0, 0] -> Nothing
			[r, g, b, a] -> Just (r, g, b, a)
			_ -> error "never occur"
	pure $ DebugUtilsLabel mnxt nm mclr

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
	Pointable ud => FnDebugUtilsMessengerCallback n n1 n2 n3 ud ->
	I.FnDebugUtilsMessengerCallback
fnDebugUtilsMessengerCallbackToC f s t dt pud =
	VkFalse <$ (f s t undefined =<< fromPointerMaybe (castPtr pud))

fromPointerMaybe :: Pointable a => Ptr a -> IO (Maybe a)
fromPointerMaybe = \case NullPtr -> pure Nothing; p -> Just <$> fromPointer p
