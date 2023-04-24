{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ViewportState.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Core

import qualified Gpu.Vulkan.Pipeline.ViewportState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineViewportStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] []

data CreateInfo mn = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoViewports :: [Viewport],
	createInfoScissors :: [Rect2d] }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn)

instance Default (CreateInfo 'Nothing) where
	def = CreateInfo {
		createInfoNext = TMaybe.N, createInfoFlags = zeroBits,
		createInfoViewports = [], createInfoScissors = [] }

createInfoToCore :: WithPoked (TMaybe.M mn) =>
	CreateInfo mn -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoViewports = (length &&& id) -> (vpc, vps),
	createInfoScissors = (length &&& id) -> (scc, scs) } f =
	withPoked' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
	allocaArray vpc \pvps ->
	pokeArray pvps vps >>
	allocaArray scc \pscs ->
	pokeArray pscs scs >>
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt',
			C.createInfoFlags = flgs,
			C.createInfoViewportCount = fromIntegral vpc,
			C.createInfoPViewports = pvps,
			C.createInfoScissorCount = fromIntegral scc,
			C.createInfoPScissors = pscs } in
	withPoked ci f
