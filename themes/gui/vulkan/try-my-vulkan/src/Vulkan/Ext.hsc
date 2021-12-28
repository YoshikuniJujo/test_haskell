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

data DebugUtilsLabel = DebugUtilsLabel {
	debugUtilsLabelName :: String,
	debugUtilsLabelColor :: Maybe (CFloat, CFloat, CFloat, CFloat) }
	deriving Show

debugUtilsLabelFromC :: I.DebugUtilsLabel -> IO DebugUtilsLabel
debugUtilsLabelFromC I.DebugUtilsLabel {
	I.debugUtilsLabelPNext = pnxt, I.debugUtilsLabelPLabelName = cnm,
	I.debugUtilsLabelColor = clr } = do
	case pnxt of
		NullPtr -> pure ()
		_ -> error "VkDebugUtilsLabelEXT: pNext must be NULL"
	nm <- peekCString cnm
	let	mclr = case clr of
			[0, 0, 0, 0] -> Nothing
			[r, g, b, a] -> Just (r, g, b, a)
			_ -> error "never occur"
	pure $ DebugUtilsLabel nm mclr

data DebugUtilsObjectNameInfo n = DebugUtilsObjectNameInfo {
	debugUtilsObjectNameInfoNext :: Maybe n,
	debugUtilsObjectNemeInfoObjectType :: ObjectType,
	debugUtilsObjectNameInfoObjectHandle :: #{type uint64_t},
	debugUtilsObjectNameInfoObjectName :: Maybe String }
	deriving Show

data DebugUtilsMessengerCallbackData n n' =
	DebugUtilsMessengerCallbackData {
		debugUtilsMessengerCallbackDataNext :: Maybe n,
		debugUtilsMessengerCallbackDataFlags ::
			I.DebugUtilsMessengerCallbackDataFlags,
		debugUtilsMessengerCallbackDataMessageIdName :: String,
		debugUtilsMessengerCallbackDataMessageIdNumber ::
			#{type int32_t},
		debugUtilsMessengerCallbackDataMessage :: String,
		debugUtilsMessengerCallbackDataQueueLabels ::
			[DebugUtilsLabel],
		debugUtilsMessengerCallbackDataCmdBufferLabels ::
			[DebugUtilsLabel],
		debugUtilsMessengerCallbackDataObjects ::
			[DebugUtilsObjectNameInfo n'] }
	deriving Show

type FnDebugUtilsMessengerCallback n n' ud =
	I.DebugUtilsMessageSeverityFlagBits ->
	I.DebugUtilsMessageTypeFlagBits ->
	DebugUtilsMessengerCallbackData n n' -> Maybe ud -> IO ()

fnDebugUtilsMessengerCallbackToC ::
	Pointable ud => FnDebugUtilsMessengerCallback n n' ud ->
	I.FnDebugUtilsMessengerCallback
fnDebugUtilsMessengerCallbackToC f s t dt pud =
	VkFalse <$ (f s t undefined =<< fromPointerMaybe (castPtr pud))

fromPointerMaybe :: Pointable a => Ptr a -> IO (Maybe a)
fromPointerMaybe = \case NullPtr -> pure Nothing; p -> Just <$> fromPointer p
