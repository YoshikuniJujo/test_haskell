{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle.Internal (
	D(..), CreateInfo(..), CreateFlags, CreateFlagBits, QueueCreateInfo(..),
	create, destroy, getQueue, waitIdle,

	Size(..) ) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke (
	WithPoked, withPokedMaybe', withPtrS, pattern NullPtr )
import Foreign.Storable.HeteroList
import Foreign.C.Enum
import Control.Arrow
import Control.Monad.Cont
import Data.Default
import Data.Bits
import Data.List
import Data.HeteroParList qualified as HeteroParList
import Data.Word

import qualified Data.Text as T

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Device.Enum
import Gpu.Vulkan.Misc.Middle.Internal

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.PhysicalDevice.Middle.Internal as PhysicalDevice
import qualified Gpu.Vulkan.PhysicalDevice.Struct as PhysicalDevice
import qualified Gpu.Vulkan.Device.Core as C
import qualified Gpu.Vulkan.Queue.Middle.Internal as Queue

import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily

#include <vulkan/vulkan.h>

newtype D = D C.D deriving Show

enum "CreateFlagBits" ''#{type VkDeviceCreateFlags}
	[''Eq, ''Show, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

type CreateFlags = CreateFlagBits

instance Default CreateFlags where def = CreateFlagsZero

data CreateInfo n qcis = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueCreateInfos :: HeteroParList.PL QueueCreateInfo qcis,
	createInfoEnabledLayerNames :: [T.Text],
	createInfoEnabledExtensionNames :: [T.Text],
	createInfoEnabledFeatures :: Maybe PhysicalDevice.Features }

deriving instance (Show n, Show (HeteroParList.PL QueueCreateInfo qcis)) =>
	Show (CreateInfo n qcis)

createInfoToCore :: (WithPoked n, WithPokedHeteroToListM qcis) =>
	CreateInfo n qcis -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueCreateInfos = qcis,
	createInfoEnabledLayerNames = (id &&& length) -> (elns, elnc),
	createInfoEnabledExtensionNames = (id &&& length) -> (eens, eenc),
	createInfoEnabledFeatures = mef } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	alloca \pci ->
		runContT (withPokedHeteroToListM (ContT . queueCreateInfoToCore) qcis) \cqcis ->
		let	qcic = length cqcis in
		allocaArray qcic \pcqcis ->
		pokeArray pcqcis cqcis >>
		textListToCStringArray elns \pcelns ->
		textListToCStringArray eens \pceens -> do
		let mk pef = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoQueueCreateInfoCount = fromIntegral qcic,
			C.createInfoPQueueCreateInfos = pcqcis,
			C.createInfoEnabledLayerCount = fromIntegral elnc,
			C.createInfoPpEnabledLayerNames = pcelns,
			C.createInfoEnabledExtensionCount = fromIntegral eenc,
			C.createInfoPpEnabledExtensionNames = pceens,
			C.createInfoPEnabledFeatures = pef }
		case mef of
			Nothing -> poke pci (mk NullPtr)
			Just ef -> alloca \p -> do
				poke p $ PhysicalDevice.featuresToCore ef
				poke pci (mk p)
		() <$ f pci

create :: (WithPoked n, WithPokedHeteroToListM qcis, WithPoked c) =>
	PhysicalDevice.P -> CreateInfo n qcis -> Maybe (AllocationCallbacks.A c) ->
	IO D
create (PhysicalDevice.P phdvc) ci mac = D <$> alloca \pdvc -> do
	createInfoToCore ci \pcci ->
		AllocationCallbacks.maybeToCore' mac \pac -> do
			r <- C.create phdvc pcci pac pdvc
			throwUnlessSuccess $ Result r
	peek pdvc

destroy :: WithPoked d => D -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (D cdvc) mac = AllocationCallbacks.maybeToCore' mac $ C.destroy cdvc

getQueue :: D -> Word32 -> Word32 -> IO Queue.Q
getQueue (D cdvc) qfi qi = ($ pure) . runContT $ Queue.Q <$> do
	pQueue <- ContT alloca
	lift do	C.getQueue cdvc qfi qi pQueue
		peek pQueue

waitIdle :: D -> IO ()
waitIdle (D d) = throwUnlessSuccess . Result =<< C.waitIdle d

data QueueCreateInfo n = QueueCreateInfo {
	queueCreateInfoNext :: Maybe n,
	queueCreateInfoFlags :: QueueCreateFlags,
	queueCreateInfoQueueFamilyIndex :: QueueFamily.Index,
	queueCreateInfoQueuePriorities :: [Float] }
	deriving Show

queueCreateInfoToCore ::
	WithPoked n => QueueCreateInfo n -> (C.QueueCreateInfo -> IO a) -> IO ()
queueCreateInfoToCore QueueCreateInfo {
	queueCreateInfoNext = mnxt,
	queueCreateInfoFlags = QueueCreateFlagBits flgs,
	queueCreateInfoQueueFamilyIndex = QueueFamily.Index qfi,
	queueCreateInfoQueuePriorities = qps
	} f = allocaArray (length qps) \pqps -> do
	pokeArray pqps qps
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
		() <$ f C.QueueCreateInfo {
			C.queueCreateInfoSType = (),
			C.queueCreateInfoPNext = pnxt',
			C.queueCreateInfoFlags = flgs,
			C.queueCreateInfoQueueFamilyIndex = qfi,
			C.queueCreateInfoQueueCount = genericLength qps,
			C.queueCreateInfoPQueuePriorities = pqps }

enum "Size" ''#{type VkDeviceSize}
		[''Show, ''Eq, ''Ord, ''Enum, ''Num, ''Real, ''Integral]
	[("WholeSize", #{const VK_WHOLE_SIZE})]
