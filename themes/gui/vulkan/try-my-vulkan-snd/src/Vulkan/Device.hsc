{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Bits
import Data.Word

import qualified Data.Text as T

import Vulkan
import Vulkan.Base
import Vulkan.Exception
import Vulkan.Exception.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device.Queue as Queue
import qualified Vulkan.PhysicalDevice as PhysicalDevice
import qualified Vulkan.PhysicalDevice.Struct as PhysicalDevice
import qualified Vulkan.Device.Core as C

#include <vulkan/vulkan.h>

newtype D = D C.D deriving Show

enum "CreateFlagBits" ''#{type VkDeviceCreateFlags}
	[''Eq, ''Show, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

type CreateFlags = CreateFlagBits

data CreateInfo n n' = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoQueueCreateInfos :: [Queue.CreateInfo n'],
	createInfoEnabledLayerNames :: [T.Text],
	createInfoEnabledExtensionNames :: [T.Text],
	createInfoEnabledFeatures :: PhysicalDevice.Features }
	deriving Show

createInfoToCore :: (Pointable n, Pointable n') =>
	CreateInfo n n' -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlagBits flgs,
	createInfoQueueCreateInfos = (id &&& length) -> (qcis, qcic),
	createInfoEnabledLayerNames = (id &&& length) -> (elns, elnc),
	createInfoEnabledExtensionNames = (id &&& length) -> (eens, eenc),
	createInfoEnabledFeatures = ef } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	cqcis <- Queue.createInfoToCore `mapM` qcis
	pcqcis <- ContT $ allocaArray qcic
	lift $ pokeArray pcqcis cqcis
	pcelns <- textListToCStringArray elns
	pceens <- textListToCStringArray eens
	pef <- ContT alloca
	lift . poke pef $ PhysicalDevice.featuresToCore ef
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

create :: (Pointable n, Pointable n2, Pointable n3) =>
	PhysicalDevice.P -> CreateInfo n n2 -> Maybe (AllocationCallbacks.A n3) ->
	IO D
create (PhysicalDevice.P phdvc) ci mac = ($ pure) . runContT $ D <$> do
	pcci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pdvc <- ContT alloca
	lift do	r <- C.create phdvc pcci pac pdvc
		throwUnlessSuccess $ Result r
		peek pdvc

destroy :: Pointable n => D -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (D cdvc) mac = ($ pure) $ runContT do
	pac <- AllocationCallbacks.maybeToCore mac
	lift $ C.destroy cdvc pac

getQueue :: D -> Word32 -> Word32 -> IO Queue
getQueue (D cdvc) qfi qi = ($ pure) . runContT $ Queue <$> do
	pQueue <- ContT alloca
	lift do	C.getQueue cdvc qfi qi pQueue
		peek pQueue

waitIdle :: D -> IO ()
waitIdle (D d) = throwUnlessSuccess . Result =<< C.waitIdle d
