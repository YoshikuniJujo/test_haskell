{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ColorBlendState.Middle.Internal (
	CreateInfo(..), CreateFlags, createInfoToCore
	) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Bits
import Data.Word
import Data.Color

import Gpu.Vulkan.Enum
import Gpu.Vulkan.Base.Middle.Internal

import qualified Gpu.Vulkan.Pipeline.ColorBlendAttachment.Middle.Internal
	as ColorBlendAttachment
import qualified Gpu.Vulkan.Pipeline.ColorBlendState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineColorBlendStateCreateFlags}
	[''Show, ''Storable, ''Eq, ''Bits] [("CreateFlagsZero", 0)]

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoLogicOpEnable :: Bool,
	createInfoLogicOp :: LogicOp,
	createInfoAttachments :: [ColorBlendAttachment.State],
	createInfoBlendConstants :: Rgba Float }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoLogicOpEnable = boolToBool32 -> loe,
	createInfoLogicOp = LogicOp lo,
	createInfoAttachments =
		length &&& (ColorBlendAttachment.stateToCore <$>) -> (ac, as),
	createInfoBlendConstants = RgbaDouble r g b a } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray ac \pas ->
	pokeArray pas as >>
	let ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoLogicOpEnable = loe,
			C.createInfoLogicOp = lo,
			C.createInfoAttachmentCount = fromIntegral ac,
			C.createInfoPAttachments = pas,
			C.createInfoBlendConstants = [r, g, b, a] } in
	withPoked ci f
