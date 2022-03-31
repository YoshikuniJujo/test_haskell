{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.Layout where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Exception
import Vulkan.Exception.Enum

import qualified Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Vulkan.Device as Device
import qualified Vulkan.DescriptorSet.Layout as DescriptorSet.Layout
import qualified Vulkan.PushConstant as PushConstant
import qualified Vulkan.Pipeline.Layout.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineLayoutCreateFlags}
	[''Show, ''Storable] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoSetLayouts :: [DescriptorSet.Layout.L],
	createInfoPushConstantRanges :: [PushConstant.Range] }
	deriving Show

createInfoToCore :: Pointable n => CreateInfo n -> ContT r IO (Ptr C.CreateInfo)
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoSetLayouts =
		(length &&& ((\(DescriptorSet.Layout.L lyt) -> lyt) <$>)) ->
			(slc, sls),
	createInfoPushConstantRanges = (length &&& id) -> (pcrc, pcrs) } = do
	(castPtr -> pnxt) <- maybeToPointer mnxt
	psls <- ContT $ allocaArray slc
	lift $ pokeArray psls sls
	ppcrs <- ContT $ allocaArray pcrc
	lift $ pokeArray ppcrs pcrs
	let C.CreateInfo_ fCreateInfo = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoSetLayoutCount = fromIntegral slc,
			C.createInfoPSetLayouts = psls,
			C.createInfoPushConstantRangeCount = fromIntegral pcrc,
			C.createInfoPPushConstantRanges = ppcrs }
	ContT $ withForeignPtr fCreateInfo

newtype L = L C.L deriving Show

create :: (Pointable n, Pointable n') =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A n') -> IO L
create (Device.D dvc) ci mac = (L <$>) . ($ pure) $ runContT do
	pci <- createInfoToCore ci
	pac <- AllocationCallbacks.maybeToCore mac
	pl <- ContT alloca
	lift do	r <- C.create dvc pci pac pl
		throwUnlessSuccess $ Result r
		peek pl

destroy :: Pointable n =>
	Device.D -> L -> Maybe (AllocationCallbacks.A n) -> IO ()
destroy (Device.D dvc) (L lyt) mac =
	($ pure) . runContT $ lift . C.destroy dvc lyt
		=<< AllocationCallbacks.maybeToCore mac
