{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout.Middle.Internal (
	L(..), CreateInfo(..), CreateFlags, create, destroy
	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import Gpu.Vulkan.AllocationCallbacks.Middle.Internal
	qualified as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle.Internal as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle.Internal as DescriptorSet.Layout
import qualified Gpu.Vulkan.PushConstant.Middle.Internal as PushConstant
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

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoSetLayouts =
		(length &&& ((\(DescriptorSet.Layout.L lyt) -> lyt) <$>)) ->
			(slc, sls),
	createInfoPushConstantRanges = (
		length &&&
		(PushConstant.rangeToCore <$>)) -> (pcrc, pcrs) } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> 
	allocaArray slc \psls ->
	pokeArray psls sls >>
	allocaArray pcrc \ppcrs ->
	pokeArray ppcrs pcrs >>
	let ci = C.CreateInfo {
		C.createInfoSType = (),
		C.createInfoPNext = pnxt',
		C.createInfoFlags = flgs,
		C.createInfoSetLayoutCount = fromIntegral slc,
		C.createInfoPSetLayouts = psls,
		C.createInfoPushConstantRangeCount = fromIntegral pcrc,
		C.createInfoPPushConstantRanges = ppcrs } in
	withPoked ci f

newtype L = L C.L deriving Show

create :: (WithPoked n, WithPoked c) =>
	Device.D -> CreateInfo n -> Maybe (AllocationCallbacks.A c) -> IO L
create (Device.D dvc) ci mac = L <$> alloca \pl -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.maybeToCore mac \pac -> do
			r <- C.create dvc pci pac pl
			throwUnlessSuccess $ Result r
	peek pl

destroy :: WithPoked d =>
	Device.D -> L -> Maybe (AllocationCallbacks.A d) -> IO ()
destroy (Device.D dvc) (L lyt) mac =
	AllocationCallbacks.maybeToCore mac $ C.destroy dvc lyt