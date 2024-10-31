{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Device.Middle.Internal (

	-- * CREATE AND DESTROY

	create, destroy, D(..), CreateInfo(..), CreateFlags, CreateFlagBits,
	QueueCreateInfo(..),

	-- * GET QUEUE AND WAIT IDLE

	getQueue, waitIdle,

	-- * SIZE

	Size(..)

	) where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke (
	WithPoked, withPoked', withPtrS, pattern NullPtr )
import Foreign.Storable.HeteroList
import Foreign.C.Enum
import Control.Arrow
import Control.Monad.Cont
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.Default
import Data.Bits
import Data.List (genericLength)
import Data.HeteroParList qualified as HeteroParList
import Data.Word
import Data.Ix

import Data.Text qualified as T
import Data.Text.Foreign.Misc

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Device.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.PhysicalDevice.Middle.Internal as PhysicalDevice
import qualified Gpu.Vulkan.PhysicalDevice.Struct as PhysicalDevice
import qualified Gpu.Vulkan.Device.Core as C
import qualified Gpu.Vulkan.Queue.Middle.Internal as Queue

import qualified Gpu.Vulkan.QueueFamily.EnumManual as QueueFamily

import Gpu.Vulkan.Device.Middle.Types

#include <vulkan/vulkan.h>

enum "CreateFlagBits" ''#{type VkDeviceCreateFlags}
	[''Eq, ''Show, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

type CreateFlags = CreateFlagBits

instance Default CreateFlags where def = CreateFlagsZero

data CreateInfo mn qcis = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoQueueCreateInfos :: HeteroParList.PL QueueCreateInfo qcis,
	createInfoEnabledLayerNames :: [T.Text],
	createInfoEnabledExtensionNames :: [T.Text],
	createInfoEnabledFeatures :: Maybe PhysicalDevice.Features }

deriving instance (Show (TMaybe.M mn), Show (HeteroParList.PL QueueCreateInfo qcis)) =>
	Show (CreateInfo mn qcis)

type family Map (f :: j -> k) xs where
	Map _f '[] = '[]
	Map f (x ': xs) = f x ': Map f xs

createInfoToCore :: (
	WithPoked (TMaybe.M mn), HeteroParList.ToListWithCM' WithPoked TMaybe.M qcis) =>
	CreateInfo mn qcis -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueCreateInfos = qcis,
	createInfoEnabledLayerNames = (id &&& length) -> (elns, elnc),
	createInfoEnabledExtensionNames = (id &&& length) -> (eens, eenc),
	createInfoEnabledFeatures = mef } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	alloca \pci ->
		runContT (HeteroParList.toListWithCM' @_ @_ @WithPoked @TMaybe.M (ContT . queueCreateInfoToCore) qcis) \cqcis ->
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

create :: (WithPoked (TMaybe.M mn), HeteroParList.ToListWithCM' WithPoked TMaybe.M qcis) =>
	PhysicalDevice.P -> CreateInfo mn qcis -> TPMaybe.M AllocationCallbacks.A mc ->
	IO D
create (PhysicalDevice.P phdvc) ci mac = D <$> alloca \pdvc -> do
	createInfoToCore ci \pcci ->
		AllocationCallbacks.mToCore mac \pac -> do
			r <- C.create phdvc pcci pac pdvc
			throwUnlessSuccess $ Result r
	peek pdvc

destroy :: D -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (D cdvc) mac = AllocationCallbacks.mToCore mac $ C.destroy cdvc

getQueue :: D -> Word32 -> Word32 -> IO Queue.Q
getQueue (D cdvc) qfi qi = Queue.Q <$> alloca \pQueue -> do
	C.getQueue cdvc qfi qi pQueue
	peek pQueue

waitIdle :: D -> IO ()
waitIdle (D d) = throwUnlessSuccess . Result =<< C.waitIdle d

data QueueCreateInfo mn = QueueCreateInfo {
	queueCreateInfoNext :: TMaybe.M mn,
	queueCreateInfoFlags :: QueueCreateFlags,
	queueCreateInfoQueueFamilyIndex :: QueueFamily.Index,
	queueCreateInfoQueuePriorities :: [Float] }

deriving instance Show (TMaybe.M mn) => Show (QueueCreateInfo mn)

queueCreateInfoToCore :: WithPoked (TMaybe.M mn) =>
	QueueCreateInfo mn -> (C.QueueCreateInfo -> IO a) -> IO ()
queueCreateInfoToCore QueueCreateInfo {
	queueCreateInfoNext = mnxt,
	queueCreateInfoFlags = QueueCreateFlagBits flgs,
	queueCreateInfoQueueFamilyIndex = QueueFamily.Index qfi,
	queueCreateInfoQueuePriorities = qps
	} f = allocaArray (length qps) \pqps -> do
	pokeArray pqps qps
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
		() <$ f C.QueueCreateInfo {
			C.queueCreateInfoSType = (),
			C.queueCreateInfoPNext = pnxt',
			C.queueCreateInfoFlags = flgs,
			C.queueCreateInfoQueueFamilyIndex = qfi,
			C.queueCreateInfoQueueCount = genericLength qps,
			C.queueCreateInfoPQueuePriorities = pqps }
