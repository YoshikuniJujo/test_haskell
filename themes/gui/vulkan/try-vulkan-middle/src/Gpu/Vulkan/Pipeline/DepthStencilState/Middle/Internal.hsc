{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.DepthStencilState.Middle.Internal (
	CreateInfo(..), CreateFlags(..), createInfoToCore
	) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Bits
import Data.Word

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Middle.Internal
import Gpu.Vulkan.Base.Middle.Internal

import qualified Gpu.Vulkan.Pipeline.DepthStencilState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineDepthStencilStateCreateFlags}
	[''Show, ''Storable, ''Eq, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoDepthTestEnable :: Bool,
	createInfoDepthWriteEnable :: Bool,
	createInfoDepthCompareOp :: CompareOp,
	createInfoDepthBoundsTestEnable :: Bool,
	createInfoStencilTestEnable :: Bool,
	createInfoFront :: StencilOpState,
	createInfoBack :: StencilOpState,
	createInfoMinDepthBounds :: Float,
	createInfoMaxDepthBounds :: Float }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoDepthTestEnable = boolToBool32 -> dte,
	createInfoDepthWriteEnable = boolToBool32 -> dwe,
	createInfoDepthCompareOp = CompareOp dco,
	createInfoDepthBoundsTestEnable = boolToBool32 -> bte,
	createInfoStencilTestEnable = boolToBool32 -> ste,
	createInfoFront = stencilOpStateToCore -> fr,
	createInfoBack = stencilOpStateToCore -> bk,
	createInfoMinDepthBounds = mndb,
	createInfoMaxDepthBounds = mxdb } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoDepthTestEnable = dte,
			C.createInfoDepthWriteEnable = dwe,
			C.createInfoDepthCompareOp = dco,
			C.createInfoDepthBoundsTestEnable = bte,
			C.createInfoStencilTestEnable = ste,
			C.createInfoFront = fr,
			C.createInfoBack = bk,
			C.createInfoMinDepthBounds = mndb,
			C.createInfoMaxDepthBounds = mxdb } in
	withPoked ci f
