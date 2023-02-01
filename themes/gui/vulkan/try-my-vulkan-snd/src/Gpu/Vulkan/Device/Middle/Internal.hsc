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
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Control.Monad.Cont
import Data.Default
import Data.Bits
import Data.List
import Data.HeteroList
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

data CreateInfo n ns = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueCreateInfos :: HeteroVarList QueueCreateInfo ns,
	createInfoEnabledLayerNames :: [T.Text],
	createInfoEnabledExtensionNames :: [T.Text],
	createInfoEnabledFeatures :: Maybe PhysicalDevice.Features }

deriving instance (Show n, Show (HeteroVarList QueueCreateInfo ns)) =>
	Show (CreateInfo n ns)

createInfoToCore :: (Pokable n, PokableToListM ns) =>
	CreateInfo n ns -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueCreateInfos = qcis,
	createInfoEnabledLayerNames = (id &&& length) -> (elns, elnc),
	createInfoEnabledExtensionNames = (id &&& length) -> (eens, eenc),
	createInfoEnabledFeatures = mef } = do
	(castPtr -> pnxt) <- ContT $ withPokedMaybe mnxt
	cqcis <- pokableToListM (ContT . queueCreateInfoToCore) qcis
	let	qcic = length cqcis
	pcqcis <- ContT $ allocaArray qcic
	lift $ pokeArray pcqcis cqcis
	pcelns <- ContT $ textListToCStringArray elns
	pceens <- ContT $ textListToCStringArray eens
	pef <- case mef of
		Nothing -> pure NullPtr
		Just ef -> do
			p <- ContT alloca
			p <$ lift (poke p $ PhysicalDevice.featuresToCore ef)
	let	 C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoQueueCreateInfoCount = fromIntegral qcic,
			C.createInfoPQueueCreateInfos = pcqcis,
			C.createInfoEnabledLayerCount = fromIntegral elnc,
			C.createInfoPpEnabledLayerNames = pcelns,
			C.createInfoEnabledExtensionCount = fromIntegral eenc,
			C.createInfoPpEnabledExtensionNames = pceens,
			C.createInfoPEnabledFeatures = pef }
	ContT $ withForeignPtr fCreateInfo

create :: (Pokable n, PokableToListM ns, WithPoked c) =>
	PhysicalDevice.P -> CreateInfo n ns -> Maybe (AllocationCallbacks.A c) ->
	IO D
create (PhysicalDevice.P phdvc) ci mac = ($ pure) . runContT $ D <$> do
	pdvc <- ContT alloca
	pcci <- createInfoToCore ci
	lift $ AllocationCallbacks.maybeToCore' mac \pac -> do
		r <- C.create phdvc pcci pac pdvc
		throwUnlessSuccess $ Result r
	lift $ peek pdvc

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
	Pokable n => QueueCreateInfo n -> (C.QueueCreateInfo -> IO a) -> IO a
queueCreateInfoToCore QueueCreateInfo {
	queueCreateInfoNext = mnxt,
	queueCreateInfoFlags = QueueCreateFlagBits flgs,
	queueCreateInfoQueueFamilyIndex = QueueFamily.Index qfi,
	queueCreateInfoQueuePriorities = qps
	} f = allocaArray (length qps) \pqps -> do
	pokeArray pqps qps
	withPokedMaybe mnxt \(castPtr -> pnxt) ->
		f C.QueueCreateInfo {
			C.queueCreateInfoSType = (),
			C.queueCreateInfoPNext = pnxt,
			C.queueCreateInfoFlags = flgs,
			C.queueCreateInfoQueueFamilyIndex = qfi,
			C.queueCreateInfoQueueCount = genericLength qps,
			C.queueCreateInfoPQueuePriorities = pqps }

enum "Size" ''#{type VkDeviceSize}
		[''Show, ''Eq, ''Ord, ''Enum, ''Num, ''Real, ''Integral]
	[("WholeSize", #{const VK_WHOLE_SIZE})]
