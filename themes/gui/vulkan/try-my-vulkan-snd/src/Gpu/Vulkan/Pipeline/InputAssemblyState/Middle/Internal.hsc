{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.InputAssemblyState.Middle.Internal (
	CreateInfo(..), createInfoToCore
	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.Bits
import Data.Word

import Gpu.Vulkan.Misc.Middle.Internal
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Pipeline.InputAssemblyState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineInputAssemblyStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoTopology :: PrimitiveTopology,
	createInfoPrimitiveRestartEnable :: Bool }
	deriving Show

createInfoToCore :: Pokable n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO a
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoTopology = PrimitiveTopology tplg,
	createInfoPrimitiveRestartEnable = pre } f =
	withPokedMaybe mnxt \(castPtr -> pnxt) ->
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoTopology = tplg,
			C.createInfoPrimitiveRestartEnable =
				boolToBool32 pre } in
	withPoked ci f
