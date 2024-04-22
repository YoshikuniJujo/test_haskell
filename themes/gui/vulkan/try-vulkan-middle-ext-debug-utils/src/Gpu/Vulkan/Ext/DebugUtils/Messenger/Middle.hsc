{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Messenger.Middle (

	-- * CREATE AND DESTROY

	create, destroy, M, CreateInfo(..), CreateFlags,

	-- ** FnCallback

	FnCallback, CallbackData(..), CallbackDataFlags

	) where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable (Storable(..))
import Foreign.Storable.PeekPoke (
	Sizable(..), Peek, peekMaybe, peekArray', withPoked, withPokedMaybe,
	WithPoked(..), withPoked', ptrS, withPtrS, Storable' )
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.HeteroParList qualified as HeteroParList
import Data.Default
import Data.Bits
import Data.Word
import Data.Int

import qualified Data.Text as T
import Data.Text.Foreign.Misc

import Gpu.Vulkan.Base.Middle.Internal
import Gpu.Vulkan.PNext.Middle.Internal
import Gpu.Vulkan.Exception.Middle
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Ext.DebugUtils.Middle.Internal
import Gpu.Vulkan.Ext.DebugUtils.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Instance.Middle.Internal as Instance
import qualified Gpu.Vulkan.Ext.DebugUtils.Messenger.Core as C

#include <vulkan/vulkan.h>

enum "CallbackDataFlags" ''#{type VkDebugUtilsMessengerCallbackDataFlagsEXT}
		[''Show, ''Storable] [("CallbackDataFlagsZero", 0)]

instance Default CallbackDataFlags where def = CallbackDataFlagsZero

data CallbackData ns = CallbackData {
	callbackDataNext :: HeteroParList.PL Maybe ns,
	callbackDataFlags :: CallbackDataFlags,
	callbackDataMessageIdName :: T.Text,
	callbackDataMessageIdNumber :: Int32,
	callbackDataMessage :: T.Text,
	callbackDataQueueLabels :: [Label],
	callbackDataCmdBufLabels :: [Label],
	callbackDataObjects :: [ObjectNameInfoNoNext] }

deriving instance Show (HeteroParList.PL Maybe ns) => Show (CallbackData ns)

callbackDataFromCore :: FindChainAll ns =>
	C.CallbackData -> IO (CallbackData ns)
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
--	mnxt <- peekMaybe $ castPtr pnxt
	mnxt <- findChainAll pnxt
	midnm <- cStringToText cmidnm
	msg <- cStringToText cmsg
	cqls <- peekArray' qlc pcqls
	qls <- labelFromCore `mapM` cqls
	ccbls <- peekArray' cblc pccbls
	cbls <- labelFromCore `mapM` ccbls
	cobjs <- peekArray' objc pcobjs
	objs <- objectNameInfoNoNextFromCore `mapM` cobjs
	pure CallbackData {
		callbackDataNext = mnxt,
		callbackDataFlags = CallbackDataFlags flgs,
		callbackDataMessageIdName = midnm,
		callbackDataMessageIdNumber = midn,
		callbackDataMessage = msg,
		callbackDataQueueLabels = qls,
		callbackDataCmdBufLabels = cbls,
		callbackDataObjects = objs }

type FnCallback cb ud =
	MessageSeverityFlagBits -> MessageTypeFlags ->
	CallbackData cb -> Maybe ud -> IO Bool

fnCallbackToCore :: (FindChainAll n, Peek ud) => FnCallback n ud -> C.FnCallback
fnCallbackToCore f sfb tf ccbd pud = do
	cbd <- callbackDataFromCore . C.CallbackData_ =<< newForeignPtr ccbd (pure ())
	mud <- peekMaybe $ castPtr pud
	boolToBool32 <$> f (MessageSeverityFlagBits sfb) (MessageTypeFlagBits tf) cbd mud

enum "CreateFlags" ''#{type VkDebugUtilsMessengerCreateFlagsEXT}
		[''Show, ''Eq, ''Storable, ''Bits] []

instance Default CreateFlags where def = zeroBits

data CreateInfo mn cb ud = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoMessageSeverity :: MessageSeverityFlags,
	createInfoMessageType :: MessageTypeFlags,
	createInfoFnUserCallback :: FnCallback cb ud,
	createInfoUserData :: Maybe ud }

instance Sizable (CreateInfo n cb ud) where
	sizeOf' = sizeOf @C.CreateInfo undefined
	alignment' = alignment @C.CreateInfo undefined

instance (WithPoked (TMaybe.M mn), FindChainAll cb, Storable' ud) =>
	WithPoked (CreateInfo mn cb ud) where
	withPoked' ci f = alloca \pcci -> do
		createInfoToCore' ci $ \cci -> poke pcci cci
		f . ptrS $ castPtr pcci

createInfoToCore' :: (
	WithPoked (TMaybe.M mn), FindChainAll cb, Storable' ud ) =>
	CreateInfo mn cb ud -> (C.CreateInfo -> IO a) -> IO ()
createInfoToCore' CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoMessageSeverity = MessageSeverityFlagBits ms,
	createInfoMessageType = MessageTypeFlagBits mt,
	createInfoFnUserCallback = cb,
	createInfoUserData = mud
	} f = withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	C.wrapCallback (fnCallbackToCore cb) >>= \pccb ->
	withPokedMaybe mud \(castPtr -> pud) -> f C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoMessageSeverity = ms,
		C.createInfoMessageType = mt,
		C.createInfoPfnUserCallback = pccb,
		C.createInfoPUserData = pud }

newtype M = M C.M deriving Show

create :: (WithPoked (TMaybe.M mn), FindChainAll cb, Storable' ud) =>
	Instance.I -> CreateInfo mn cb ud ->
	TPMaybe.M AllocationCallbacks.A mc -> IO M
create (Instance.I ist) ci mac = M <$> alloca \pmsngr -> do
	createInfoToCore' ci \cci ->
		withPoked cci \pcci ->
		AllocationCallbacks.mToCore mac \pac ->
		throwUnlessSuccess . Result =<< C.create ist pcci pac pmsngr
	peek pmsngr

destroy :: Instance.I -> M -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Instance.I ist) (M msgr) mac =
	AllocationCallbacks.mToCore mac $ C.destroy ist msgr
