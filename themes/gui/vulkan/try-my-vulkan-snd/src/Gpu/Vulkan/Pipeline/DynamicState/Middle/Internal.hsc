{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.DynamicState.Middle.Internal (
	CreateInfo(..), CreateFlags(..), createInfoToCore
	) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Storable.PeekPoke
import Foreign.C.Enum
import Control.Arrow
import Data.Word

import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Pipeline.DynamicState.Core as C

#include <vulkan/vulkan.h>

enum "CreateFlags" ''#{type VkPipelineDynamicStateCreateFlags}
	[''Show, ''Storable] [("CreateFlagsZero", 0)]

data CreateInfo n = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: CreateFlags,
	createInfoDynamicStates :: [DynamicState] }
	deriving Show

createInfoToCore :: Pokable n =>
	CreateInfo n -> (Ptr C.CreateInfo -> IO a) -> IO a
createInfoToCore CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = CreateFlags flgs,
	createInfoDynamicStates = (
		length &&&
		((\(DynamicState ds) -> ds) <$>) ) -> (dsc, dss) } f =
	withPokedMaybe mnxt \(castPtr -> pnxt) ->
	allocaArray dsc \pdss ->
	pokeArray pdss dss >>
	let	ci = C.CreateInfo {
			C.createInfoSType = (),
			C.createInfoPNext = pnxt,
			C.createInfoFlags = flgs,
			C.createInfoDynamicStateCount = fromIntegral dsc,
			C.createInfoPDynamicStates = pdss } in
	withPoked ci f
