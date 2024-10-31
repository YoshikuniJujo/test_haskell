{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineLayout.Middle.Internal (
	P(..), CreateInfo(..), CreateFlags, create, destroy
	) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
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
import qualified Gpu.Vulkan.PipelineLayout.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineLayoutCreateFlags}
	[''Show, ''Storable, ''Eq, ''Bits] [("CreateFlagsZero", 0)]

instance Default CreateFlags where def = CreateFlagsZero

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoSetLayouts :: [DescriptorSet.Layout.D],
	createInfoPushConstantRanges :: [PushConstant.Range] }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoSetLayouts =
		(length &&& ((\(DescriptorSet.Layout.D lyt) -> lyt) <$>)) ->
			(slc, sls),
	createInfoPushConstantRanges = (
		length &&&
		(PushConstant.rangeToCore <$>)) -> (pcrc, pcrs) } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') -> 
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

newtype P = P C.P deriving Show

create :: WithPoked (TMaybe.M mn) =>
	Device.D -> CreateInfo mn -> TPMaybe.M AllocationCallbacks.A mc -> IO P
create (Device.D dvc) ci mac = P <$> alloca \pl -> do
	createInfoToCore ci \pci ->
		AllocationCallbacks.mToCore mac \pac -> do
			r <- C.create dvc pci pac pl
			throwUnlessSuccess $ Result r
	peek pl

destroy :: Device.D -> P -> TPMaybe.M AllocationCallbacks.A md -> IO ()
destroy (Device.D dvc) (P lyt) mac =
	AllocationCallbacks.mToCore mac $ C.destroy dvc lyt
