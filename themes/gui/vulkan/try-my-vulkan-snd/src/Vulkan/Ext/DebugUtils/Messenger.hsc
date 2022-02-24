{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext.DebugUtils.Messenger where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Data.Word
import Data.Int

import qualified Data.Text as T

import Vulkan.Base
import Vulkan.Ext.DebugUtils
import Vulkan.Ext.DebugUtils.Message.Enum

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
	callbackDataObjects :: [ObjectNameInfo n4] }
	deriving Show

callbackDataFromCore ::
	(Pointable n, Pointable n2, Pointable n3, Pointable n4) =>
	C.CallbackData -> IO (CallbackData n n2 n3 n4)
callbackDataFromCore C.CallbackData {
	C.callbackDataPNext = pnxt,
	C.callbackDataFlags = flgs,
	C.callbackDataPMessageIdName = cmidnm,
	C.callbackDataMessageIdNumber = midn,
	C.callbackDataPMessage = cmsg,
	C.callbackDataQueueLabelCount = fromIntegral -> qlc,
	C.callbackDataPQueueLabels = pcqls,
	C.callbackDataCmdBufLabelCount = fromIntegral -> cblc,
	C.callbackDataPCmdBufLabels = pccbls,
	C.callbackDataObjectCount = fromIntegral -> objc,
	C.callbackDataPObjects = pcobjs } = do
	mnxt <- pointerToMaybe $ castPtr pnxt
	midnm <- cstringToText cmidnm
	msg <- cstringToText cmsg
	cqls <- peekArray qlc pcqls
	qls <- labelFromCore `mapM` cqls
	ccbls <- peekArray cblc pccbls
	cbls <- labelFromCore `mapM` ccbls
	cobjs <- peekArray objc pcobjs
	objs <- objectNameInfoFromCore `mapM` cobjs
	pure CallbackData {
		callbackDataNext = mnxt,
		callbackDataFlags = CallbackDataFlags flgs,
		callbackDataMessageIdName = midnm,
		callbackDataMessageIdNumber = midn,
		callbackDataMessage = msg,
		callbackDataQueueLabels = qls,
		callbackDataCmdBufLabels = cbls,
		callbackDataObjects = objs }

type FnCallback n n2 n3 n4 ud =
	SeverityFlagBits -> TypeFlags -> CallbackData n n2 n3 n4 -> Maybe ud ->
	IO Bool

fnCallbackToCore ::
	(Pointable n, Pointable n2, Pointable n3, Pointable n4, Pointable ud) =>
	FnCallback n n2 n3 n4 ud -> C.FnCallback
fnCallbackToCore f sfb tf ccbd pud = do
	cbd <- callbackDataFromCore . C.CallbackData_ =<< newForeignPtr ccbd (pure ())
	mud <- pointerToMaybe $ castPtr pud
	boolToBool32 <$> f (SeverityFlagBits sfb) (TypeFlagBits tf) cbd mud
