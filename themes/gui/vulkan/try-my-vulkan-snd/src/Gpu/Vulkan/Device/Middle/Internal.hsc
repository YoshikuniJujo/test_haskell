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
import Foreign.Pointable
import Foreign.C.Enum
import Control.Arrow
import Control.Monad.Cont
import Data.Default
import Data.Bits
import Data.List
import Data.HeteroList hiding (length)
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

createInfoToCore :: (Pointable n, PointableToListM ns) =>
	CreateInfo n ns -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueCreateInfos = qcis,
	createInfoEnabledLayerNames = (id &&& length) -> (elns, elnc),
	createInfoEnabledExtensionNames = (id &&& length) -> (eens, eenc),
	createInfoEnabledFeatures = mef } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	cqcis <- pointableToListM queueCreateInfoToCore qcis
	let	qcic = length cqcis
	pcqcis <- ContT $ allocaArray qcic
	lift $ pokeArray pcqcis cqcis
	pcelns <- textListToCStringArray elns
	pceens <- textListToCStringArray eens
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

create :: (Pointable n, PointableToListM ns, Pointable c) =>
	PhysicalDevice.P -> CreateInfo n ns -> Maybe (AllocationCallbacks.A c) ->
	IO D
create (PhysicalDevice.P phdvc) ci mac = ($ pure) . runContT $ D <$> do
	pcci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pdvc <- ContT alloca
	lift do	r <- C.create phdvc pcci pac pdvc
		throwUnlessSuccess $ Result r
		peek pdvc

destroy :: Pointable d => D -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (D cdvc) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy cdvc pac

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

queueCreateInfoToCore :: Pointable n => QueueCreateInfo n -> ContT r IO C.QueueCreateInfo
queueCreateInfoToCore QueueCreateInfo {
	queueCreateInfoNext = mnxt,
	queueCreateInfoFlags = QueueCreateFlagBits flgs,
	queueCreateInfoQueueFamilyIndex = QueueFamily.Index qfi,
	queueCreateInfoQueuePriorities = qps
	} = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	pqps <- ContT $ allocaArray (length qps)
	lift $ pokeArray pqps qps
	pure C.QueueCreateInfo {
		C.queueCreateInfoSType = (),
		C.queueCreateInfoPNext = pnxt,
		C.queueCreateInfoFlags = flgs,
		C.queueCreateInfoQueueFamilyIndex = qfi,
		C.queueCreateInfoQueueCount = genericLength qps,
		C.queueCreateInfoPQueuePriorities = pqps }

enum "Size" ''#{type VkDeviceSize}
		[''Show, ''Eq, ''Ord, ''Enum, ''Num, ''Real, ''Integral]
	[("WholeSize", #{const VK_WHOLE_SIZE})]
