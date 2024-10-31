{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.InputAssemblyState.Middle.Internal (
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

import qualified Gpu.Vulkan.Pipeline.InputAssemblyState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineInputAssemblyStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoTopology :: PrimitiveTopology,
	createInfoPrimitiveRestartEnable :: Bool }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoTopology = PrimitiveTopology tplg,
	createInfoPrimitiveRestartEnable = pre } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoTopology = tplg,
			C.createInfoPrimitiveRestartEnable =
				boolToBool32 pre } in
	withPoked ci f
