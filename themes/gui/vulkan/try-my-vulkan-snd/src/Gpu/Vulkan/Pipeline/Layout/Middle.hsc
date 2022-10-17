{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout.Middle where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Enum
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle.Internal as DescriptorSet.Layout
import qualified Gpu.Vulkan.PushConstant.Middle as PushConstant
import qualified Gpu.Vulkan.Pipeline.Layout.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineLayoutCreateFlags}
	[''Show, ''Storable, ''Eq, ''Bits] [("CreateFlagsZero", 0)]

instance Default CreateFlags where def = CreateFlagsZero

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
	createInfoPushConstantRanges =
		(length &&& (PushConstant.rangeToCore <$>)) -> (pcrc, pcrs)
	} = do
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
