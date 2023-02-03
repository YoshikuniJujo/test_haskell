{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Ext.DebugUtils.Messenger.Middle (

	-- * Type

	M,

	-- * Create and Destroy

	create, destroy, CreateInfo(..), CreateFlags,
	FnCallback, CallbackData(..), CallbackDataFlags ) where

import Foreign.Ptr
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable (Storable(..))
import Foreign.Storable.PeekPoke (
	Sizable(..), Peek, peekMaybe, peekArray', withPoked, withPokedMaybe,
	WithPoked(..), withPokedMaybe', ptrS, withPtrS, Storable' )
import Foreign.C.Enum
import Data.Default
import Data.Bits
import Data.Word
import Data.Int

import qualified Data.Text as T

import Gpu.Vulkan.Misc.Middle.Internal
import Gpu.Vulkan.Exception.Middle.Internal
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

data CallbackData n ql cbl obj = CallbackData {
	callbackDataNext :: Maybe n,
	callbackDataFlags :: CallbackDataFlags,
	callbackDataMessageIdName :: T.Text,
	callbackDataMessageIdNumber :: Int32,
	callbackDataMessage :: T.Text,
	callbackDataQueueLabels :: [Label ql],
	callbackDataCmdBufLabels :: [Label cbl],
	callbackDataObjects :: [ObjectNameInfo obj] }
	deriving Show

callbackDataFromCore :: (Peek n, Peek ql, Peek cbl, Peek obj) =>
	C.CallbackData -> IO (CallbackData n ql cbl obj)
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
	mnxt <- peekMaybe $ castPtr pnxt
	midnm <- cstrToText cmidnm
	msg <- cstrToText cmsg
	cqls <- peekArray' qlc pcqls
	qls <- labelFromCore `mapM` cqls
	ccbls <- peekArray' cblc pccbls
	cbls <- labelFromCore `mapM` ccbls
	cobjs <- peekArray' objc pcobjs
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

type FnCallback cb ql cbl obj ud =
	MessageSeverityFlagBits -> MessageTypeFlags ->
	CallbackData cb ql cbl obj -> Maybe ud -> IO Bool

fnCallbackToCore :: (Peek n, Peek ql, Peek cbl, Peek obj, Peek ud) =>
	FnCallback n ql cbl obj ud -> C.FnCallback
fnCallbackToCore f sfb tf ccbd pud = do
	cbd <- callbackDataFromCore . C.CallbackData_ =<< newForeignPtr ccbd (pure ())
	mud <- peekMaybe $ castPtr pud
	boolToBool32 <$> f (MessageSeverityFlagBits sfb) (MessageTypeFlagBits tf) cbd mud

enum "CreateFlags" ''#{type VkDebugUtilsMessengerCreateFlagsEXT}
		[''Show, ''Eq, ''Storable, ''Bits] []

instance Default CreateFlags where def = zeroBits

data CreateInfo n cb ql cbl obj ud = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoMessageSeverity :: MessageSeverityFlags,
	createInfoMessageType :: MessageTypeFlags,
	createInfoFnUserCallback :: FnCallback cb ql cbl obj ud,
	createInfoUserData :: Maybe ud }

instance Sizable (CreateInfo n cb ql cbl obj ud) where
	sizeOf' = sizeOf @C.CreateInfo undefined
	alignment' = alignment @C.CreateInfo undefined

instance (WithPoked n, Peek cb, Peek ql, Peek cbl, Peek obj, Storable' ud) =>
	WithPoked (CreateInfo n cb ql cbl obj ud) where
	withPoked' ci f = alloca \pcci -> do
		createInfoToCore' ci $ \cci -> poke pcci cci
		f . ptrS $ castPtr pcci

createInfoToCore' :: (
	WithPoked n, Peek cb, Peek ql, Peek cbl, Peek obj, Storable' ud ) =>
	CreateInfo n cb ql cbl obj ud -> (C.CreateInfo -> IO a) -> IO ()
createInfoToCore' CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoMessageSeverity = MessageSeverityFlagBits ms,
	createInfoMessageType = MessageTypeFlagBits mt,
	createInfoFnUserCallback = cb,
	createInfoUserData = mud
	} f = withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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

create :: (
	WithPoked n, Peek cb, Peek ql, Peek cbl, Peek obj, Storable' ud,
	WithPoked c ) =>
	Instance.I -> CreateInfo n cb ql cbl obj ud ->
	Maybe (AllocationCallbacks.A c) -> IO M
create (Instance.I ist) ci mac = M <$> alloca \pmsngr -> do
	createInfoToCore' ci \cci ->
		withPoked cci \pcci ->
		AllocationCallbacks.maybeToCore' mac \pac ->
		throwUnlessSuccess . Result =<< C.create ist pcci pac pmsngr
	peek pmsngr

destroy :: WithPoked d =>
	Instance.I -> M -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Instance.I ist) (M msgr) mac =
	AllocationCallbacks.maybeToCore' mac $ C.destroy ist msgr
