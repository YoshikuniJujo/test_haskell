{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Device where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Enum
import Control.Arrow
import Control.Monad.Cont
import Data.Bits
import Data.Word

import qualified Data.Text as T

import Vulkan.Base

import qualified Vulkan.Device.Queue as Queue
import qualified Vulkan.PhysicalDevice.Struct as PhysicalDevice
import qualified Vulkan.Device.Core as C

#include <vulkan/vulkan.h>

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
	CreateInfo n n' -> ContT r IO C.CreateInfo
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
	pure C.CreateInfo {
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
