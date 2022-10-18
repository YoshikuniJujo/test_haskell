{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Messenger.Middle where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Monad.Cont
import Data.Default
import Data.Word
import Data.Int

import qualified Data.Text as T

import Gpu.Vulkan.Base
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Ext.DebugUtils.Middle.Internal
import Gpu.Vulkan.Ext.DebugUtils.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Middle as Instance
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger.Core as C

#include <vulkan/vulkan.h>

enum "CallbackDataFlags" ''#{type VkDebugUtilsMessengerCallbackDataFlagsEXT}
		[''Show, ''Storable] [("CallbackDataFlagsZero", 0)]

instance Default CallbackDataFlags where def = CallbackDataFlagsZero

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
	(Storable n, Storable n2, Storable n3, Storable n4) =>
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
	midnm <- cstrToText cmidnm
	msg <- cstrToText cmsg
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
	MessageSeverityFlagBits -> MessageTypeFlags -> CallbackData n n2 n3 n4 -> Maybe ud ->
	IO Bool

fnCallbackToCore ::
	(Storable n, Storable n2, Storable n3, Storable n4, Storable ud) =>
	FnCallback n n2 n3 n4 ud -> C.FnCallback
fnCallbackToCore f sfb tf ccbd pud = do
	cbd <- callbackDataFromCore . C.CallbackData_ =<< newForeignPtr ccbd (pure ())
	mud <- pointerToMaybe $ castPtr pud
	boolToBool32 <$> f (MessageSeverityFlagBits sfb) (MessageTypeFlagBits tf) cbd mud

enum "CreateFlags" ''#{type VkDebugUtilsMessengerCreateFlagsEXT}
		[''Show, ''Storable] [("CreateFlagsZero", 0)]

instance Default CreateFlags where def = CreateFlagsZero

data CreateInfo n n2 n3 n4 n5 ud = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoMessageSeverity :: MessageSeverityFlags,
	createInfoMessageType :: MessageTypeFlags,
	createInfoFnUserCallback :: FnCallback n2 n3 n4 n5 ud,
	createInfoUserData :: Maybe ud }

instance
	(Pointable n, Storable n2, Storable n3, Storable n4, Storable n5,
		Pointable ud, Storable ud) =>
	Pointable (CreateInfo n n2 n3 n4 n5 ud) where
	withPointer = runContT . (castPtr <$>) . createInfoToCore

createInfoToCore ::
	(Pointable n, Storable n2, Storable n3, Storable n4, Storable n5,
		Pointable ud, Storable ud) =>
	CreateInfo n n2 n3 n4 n5 ud -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoMessageSeverity = MessageSeverityFlagBits ms,
	createInfoMessageType = MessageTypeFlagBits mt,
	createInfoFnUserCallback = cb,
	createInfoUserData = mud
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pccb <- lift . C.wrapCallback $ fnCallbackToCore cb
	(castPtr -> pud) <- maybeToPointer mud
	let	C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoMessageSeverity = ms,
			C.createInfoMessageType = mt,
			C.createInfoPfnUserCallback = pccb,
			C.createInfoPUserData = pud }
	ContT $ withForeignPtr fCreateInfo

newtype M = M C.M deriving Show

create :: (
	Pointable n, Storable n2, Storable n3, Storable n4, Storable n5,
	Pointable n6, Pointable ud, Storable ud ) =>
	Instance.I -> CreateInfo n n2 n3 n4 n5 ud ->
	Maybe (AllocationCallbacks.A n6) -> IO M
create (Instance.I ist) ci mac = ($ pure) . runContT $ M <$> do
	cci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pmsngr <- ContT alloca
	lift do	r <- C.create ist cci pac pmsngr
		throwUnlessSuccess $ Result r
		peek pmsngr

destroy :: Pointable n =>
	Instance.I -> M -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Instance.I ist) (M msgr) mac = ($ pure) . runContT
	$ lift . C.destroy ist msgr =<< AllocationCallbacks.maybeToCore mac
