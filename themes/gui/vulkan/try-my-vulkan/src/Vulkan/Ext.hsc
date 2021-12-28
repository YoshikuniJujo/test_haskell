{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal.Array
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

data DebugUtilsMessengerCallbackData =
	DebugUtilsMessengerCallbackData {
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

debugUtilsMessengerCallbackDataFromC ::
	I.DebugUtilsMessengerCallbackData -> IO DebugUtilsMessengerCallbackData
debugUtilsMessengerCallbackDataFromC I.DebugUtilsMessengerCallbackData {
	I.debugUtilsMessengerCallbackDataPNext = pnxt,
	I.debugUtilsMessengerCallbackDataFlags = flgs,
	I.debugUtilsMessengerCallbackDataPMessageIdName = cnm,
	I.debugUtilsMessengerCallbackDataMessageIdNumber = num,
	I.debugUtilsMessengerCallbackDataPMessage = cmsg,
	I.debugUtilsMessengerCallbackDataQueueLabelCount = qlcnt,
	I.debugUtilsMessengerCallbackDataPQueueLabels = pqls,
	I.debugUtilsMessengerCallbackDataCmdBufLabelCount = cblcnt,
	I.debugUtilsMessengerCallbackDataPCmdBufLabels = pcbls,
	I.debugUtilsMessengerCallbackDataObjectCount = objcnt,
	I.debugUtilsMessengerCallbackDataPObjects = pobjs } = do
	case pnxt of
		NullPtr -> pure ()
		_ -> error $ "VkDebugUtilsMessengerCallbackDataEXT:" ++
			" pNext must be NULL"
	nm <- peekCString cnm
	msg <- peekCString cmsg
	qls <- (debugUtilsLabelFromC `mapM`)
		=<< peekArray (fromIntegral qlcnt) pqls
	bls <- (debugUtilsLabelFromC `mapM`)
		=<< peekArray (fromIntegral cblcnt) pcbls
	objs <- (debugUtilsObjectNameInfoFromC `mapM`)
		=<< peekArray (fromIntegral objcnt) pobjs
	pure $ DebugUtilsMessengerCallbackData flgs nm num msg qls bls objs

type FnDebugUtilsMessengerCallback ud =
	I.DebugUtilsMessageSeverityFlagBits ->
	I.DebugUtilsMessageTypeFlagBits ->
	DebugUtilsMessengerCallbackData -> Maybe ud -> IO ()

fnDebugUtilsMessengerCallbackToC ::
	Pointable ud => FnDebugUtilsMessengerCallback ud ->
	I.FnDebugUtilsMessengerCallback
fnDebugUtilsMessengerCallbackToC f s t pdt pud = do
	cdt <- I.DebugUtilsMessengerCallbackData_
		<$> newForeignPtr pdt (pure ())
	dt <- debugUtilsMessengerCallbackDataFromC cdt
	VkFalse <$ (f s t dt =<< fromPointerMaybe (castPtr pud))

fromPointerMaybe :: Pointable a => Ptr a -> IO (Maybe a)
fromPointerMaybe = \case NullPtr -> pure Nothing; p -> Just <$> fromPointer p
