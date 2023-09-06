{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Ext where

import Prelude hiding (Bool(..))

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
-- import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.String
import Control.Exception
import Data.Word
import Data.Int

import Vulkan
import Vulkan.Instance
import Vulkan.Base
import Vulkan.Exception
import Vulkan.AllocationCallbacks

import qualified Vulkan.Ext.Internal as I
import qualified Vulkan.AllocationCallbacks.Internal as I

#include <vulkan/vulkan.h>

debugUtilsExtensionName :: String
debugUtilsExtensionName = #{const_str VK_EXT_DEBUG_UTILS_EXTENSION_NAME}

data DebugUtilsLabel = DebugUtilsLabel {
	debugUtilsLabelName :: String,
	debugUtilsLabelColor :: Maybe (#{type float},
		#{type float}, #{type float}, #{type float}) }
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
	False <$ (f s t dt =<< fromPointerMaybe (castPtr pud))

fromPointerMaybe :: Pointable a => Ptr a -> IO (Maybe a)
fromPointerMaybe = \case NullPtr -> pure Nothing; p -> Just <$> fromPointer p

data DebugUtilsMessengerCreateInfo n ud = DebugUtilsMessengerCreateInfo {
	debugUtilsMessengerCreateInfoNext :: Maybe n,
	debugUtilsMessengerCreateInfoFlags :: I.DebugUtilsMessengerCreateFlags,
	debugUtilsMessengerCreateInfoMessageSeverity ::
		I.DebugUtilsMessageSeverityFlagBits,
	debugUtilsMessengerCreateInfoMessageType ::
		I.DebugUtilsMessageTypeFlagBits,
	debugUtilsMessengerCreateInfoFnUserCallback ::
		FnDebugUtilsMessengerCallback ud,
	debugUtilsMessengerCreateInfoUserData :: Maybe ud }

debugUtilsMessengerCreateInfoToC :: (Pointable n, Pointable ud) =>
	DebugUtilsMessengerCreateInfo n ud ->
	(I.DebugUtilsMessengerCreateInfo -> IO a) -> IO a
debugUtilsMessengerCreateInfoToC DebugUtilsMessengerCreateInfo {
	debugUtilsMessengerCreateInfoNext = mnxt,
	debugUtilsMessengerCreateInfoFlags = flgs,
	debugUtilsMessengerCreateInfoMessageSeverity = ms,
	debugUtilsMessengerCreateInfoMessageType = mt,
	debugUtilsMessengerCreateInfoFnUserCallback = cucbk,
	debugUtilsMessengerCreateInfoUserData = mud } f =
	withPointerMaybe mnxt \pnxt -> withPointerMaybe mud \pud -> do
		fpucbk <- I.wrapFnDebugUtilsMessengerCallback
			$ fnDebugUtilsMessengerCallbackToC cucbk
		f $ I.DebugUtilsMessengerCreateInfo () (castPtr pnxt) flgs ms mt fpucbk (castPtr pud)

instance (Pointable n, Pointable ud) => Pointable (DebugUtilsMessengerCreateInfo n ud) where
	withPointer ci f =
		debugUtilsMessengerCreateInfoToC ci \cci -> alloca \pcci -> do
			poke pcci cci
			f $ castPtr pcci
	fromPointer _ = error "yet"

type FnCreateDebugUtilsMessenger n ud a =
	Instance -> DebugUtilsMessengerCreateInfo n ud ->
	Maybe (AllocationCallbacks a) -> IO I.DebugUtilsMessenger

fnCreateDebugUtilsMessengerFromC :: (Pointable n, Pointable ud, Pointable a) =>
	I.FnCreateDebugUtilsMessenger -> FnCreateDebugUtilsMessenger n ud a
fnCreateDebugUtilsMessengerFromC f (Instance pist) ci mac =
	I.DebugUtilsMessenger <$> debugUtilsMessengerCreateInfoToC ci
			\(I.DebugUtilsMessengerCreateInfo_ fci) ->
		withForeignPtr fci \pci ->
			withAllocationCallbacksPtrMaybe mac \pac -> alloca \pdum -> do
				r <- f pist pci pac pdum
				throwUnlessSuccess r
				peek pdum

withAllocationCallbacksPtrMaybe :: Pointable a =>
	Maybe (AllocationCallbacks a) ->
	(Ptr I.AllocationCallbacks -> IO b) -> IO b
withAllocationCallbacksPtrMaybe mac f = case mac of
	Nothing -> f NullPtr; Just ac -> withAllocationCallbacksPtr ac f

createDebugUtilsMessenger :: (Pointable n, Pointable ud, Pointable a) =>
	Instance -> DebugUtilsMessengerCreateInfo n ud ->
	Maybe (AllocationCallbacks a) -> IO I.DebugUtilsMessenger
createDebugUtilsMessenger ist@(Instance pist) ci ac =
	withCString "vkCreateDebugUtilsMessengerEXT" \cfnnm ->
		I.c_vkGetInstanceProcAddr pist cfnnm >>= \case
			NullFunPtr -> throw ErrorExtensionNotPresent
			pf -> fnCreateDebugUtilsMessengerFromC
				(I.mkFnCreateDebugUtilsMessenger pf) ist ci ac

type FnDestroyDebugUtilsMessenger a = Instance ->
	I.DebugUtilsMessenger -> Maybe (AllocationCallbacks a) -> IO ()

fnDestroyDebugUtilsMessengerFromC :: Pointable a =>
	I.FnDestroyDebugUtilsMessenger -> FnDestroyDebugUtilsMessenger a
fnDestroyDebugUtilsMessengerFromC
	f (Instance pist) (I.DebugUtilsMessenger pdum) mac =
	withAllocationCallbacksPtrMaybe mac \pac -> f pist pdum pac

destroyDebugUtilsMessenger :: Pointable a => Instance ->
	I.DebugUtilsMessenger -> Maybe (AllocationCallbacks a) -> IO ()
destroyDebugUtilsMessenger ist@(Instance pist) dum mac =
	withCString "vkDestroyDebugUtilsMessengerEXT" \cfnnm ->
		I.c_vkGetInstanceProcAddr pist cfnnm >>= \case
			NullFunPtr -> throw ErrorExtensionNotPresent
			pf -> fnDestroyDebugUtilsMessengerFromC
				(I.mkFnDestroyDebugUtilsMessenger pf)
				ist dum mac
