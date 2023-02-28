{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.ViewportState.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.Default
import Data.Bits
import Data.Word

import Gpu.Vulkan.Core

import qualified Gpu.Vulkan.Pipeline.ViewportState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineViewportStateCreateFlags}
	[''Show, ''Eq, ''Storable, ''Bits] []

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoViewports :: [Viewport],
	createInfoScissors :: [Rect2d] }
	deriving Show

instance Default (CreateInfo n) where
	def = CreateInfo {
		createInfoNext = Nothing, createInfoFlags = zeroBits,
		createInfoViewports = [], createInfoScissors = [] }

createInfoToCore :: WithPoked n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO ()
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoViewports = (length &&& id) -> (vpc, vps),
	createInfoScissors = (length &&& id) -> (scc, scs) } f =
	withPokedMaybe' mnxt \pnxt -> withPtrS pnxt \(castPtr -> pnxt') ->
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
