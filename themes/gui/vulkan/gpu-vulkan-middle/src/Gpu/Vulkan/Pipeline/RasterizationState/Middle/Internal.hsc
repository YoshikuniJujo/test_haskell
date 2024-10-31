{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.RasterizationState.Middle.Internal (
	CreateInfo(..), CreateFlags, createInfoToCore
	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Bits
import Data.Word

import Gpu.Vulkan.Base.Middle.Internal
import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Pipeline.RasterizationState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineRasterizationStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoDepthClampEnable :: Bool,
	createInfoRasterizerDiscardEnable :: Bool,
	createInfoPolygonMode :: PolygonMode,
	createInfoCullMode :: CullModeFlags,
	createInfoFrontFace :: FrontFace,
	createInfoDepthBiasEnable :: Bool,
	createInfoDepthBiasConstantFactor :: Float,
	createInfoDepthBiasClamp :: Float,
	createInfoDepthBiasSlopeFactor :: Float,
	createInfoLineWidth :: Float }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoDepthClampEnable = boolToBool32 -> dce,
	createInfoRasterizerDiscardEnable = boolToBool32 -> rde,
	createInfoPolygonMode = PolygonMode pm,
	createInfoCullMode = CullModeFlagBits cm,
	createInfoFrontFace = FrontFace ff,
	createInfoDepthBiasEnable = boolToBool32 -> dbe,
	createInfoDepthBiasConstantFactor = dbcf,
	createInfoDepthBiasClamp = dbc,
	createInfoDepthBiasSlopeFactor = dbsf,
	createInfoLineWidth = lw } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoDepthClampEnable = dce,
			C.createInfoRasterizerDiscardEnable = rde,
			C.createInfoPolygonMode = pm,
			C.createInfoCullMode = cm,
			C.createInfoFrontFace = ff,
			C.createInfoDepthBiasEnable = dbe,
			C.createInfoDepthBiasConstantFactor = dbcf,
			C.createInfoDepthBiasClamp = dbc,
			C.createInfoDepthBiasSlopeFactor = dbsf,
			C.createInfoLineWidth = lw } in
	withPoked ci f
