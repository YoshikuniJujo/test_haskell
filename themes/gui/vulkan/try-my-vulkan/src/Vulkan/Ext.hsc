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

data DebugUtilsObjectNameInfo = DebugUtilsObjectNameInfo {
	debugUtilsObjectNemeInfoObjectType :: ObjectType,
	debugUtilsObjectNameInfoObjectHandle :: #{type uint64_t},
	debugUtilsObjectNameInfoObjectName :: Maybe String }
	deriving Show

debugUtilsObjectNameInfoFromC ::
	I.DebugUtilsObjectNameInfo -> IO DebugUtilsObjectNameInfo
debugUtilsObjectNameInfoFromC I.DebugUtilsObjectNameInfo {
	I.debugUtilsObjectNameInfoPNext = pnxt,
	I.debugUtilsObjectNameInfoObjectType = ot,
	I.debugUtilsObjectNameInfoObjectHandle = oh,
	I.debugUtilsObjectNameInfoPObjectName = cnm } = do
	case pnxt of
		NullPtr -> pure ()
		_ -> error "VkDebugUtilsObjectNameInfoEXT: pNext must be NULL"
	nm <- peekCStringMaybe cnm
	pure $ DebugUtilsObjectNameInfo ot oh nm

peekCStringMaybe :: CString -> IO (Maybe String)
peekCStringMaybe = \case NullPtr -> pure Nothing; p -> Just <$> peekCString p

data DebugUtilsMessengerCallbackData n =
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
			[DebugUtilsObjectNameInfo] }
	deriving Show

type FnDebugUtilsMessengerCallback n ud =
	I.DebugUtilsMessageSeverityFlagBits ->
	I.DebugUtilsMessageTypeFlagBits ->
	DebugUtilsMessengerCallbackData n -> Maybe ud -> IO ()

fnDebugUtilsMessengerCallbackToC ::
	Pointable ud => FnDebugUtilsMessengerCallback n ud ->
	I.FnDebugUtilsMessengerCallback
fnDebugUtilsMessengerCallbackToC f s t dt pud =
	VkFalse <$ (f s t undefined =<< fromPointerMaybe (castPtr pud))

fromPointerMaybe :: Pointable a => Ptr a -> IO (Maybe a)
fromPointerMaybe = \case NullPtr -> pure Nothing; p -> Just <$> fromPointer p
