{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass (
	R, createNew, CreateInfoNew(..), BeginInfo(..) ) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.RenderPass.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import Gpu.Vulkan.RenderPass.Enum
import qualified Gpu.Vulkan.RenderPass.Middle as M
import qualified Gpu.Vulkan.Attachment as Attachment

import qualified Gpu.Vulkan.Subpass.Middle as Subpass

createNew :: (
	Attachment.DescriptionListToMiddle fmts, WithPoked (TMaybe.M mn),
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfoNew mn fmts ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . R s -> IO a) -> IO a
createNew (Device.D dvc) ci (AllocationCallbacks.toMiddle -> macd) f = bracket(M.create dvc (createInfoFromNew ci) macd)
		(\r -> M.destroy dvc r macd) (f . R)

data CreateInfoNew mn fmts = CreateInfoNew {
	createInfoNextNew :: TMaybe.M mn,
	createInfoFlagsNew :: CreateFlags,
	createInfoAttachmentsNew ::
		HeteroParList.PL Attachment.Description fmts,
	createInfoSubpassesNew :: [Subpass.Description],
	createInfoDependenciesNew :: [Subpass.Dependency] }

createInfoFromNew :: Attachment.DescriptionListToMiddle fmts =>
	CreateInfoNew n fmts -> M.CreateInfo n
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoAttachmentsNew = atts,
	createInfoSubpassesNew = spss,
	createInfoDependenciesNew = dps } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoAttachments = Attachment.descriptionListToMiddle atts,
	M.createInfoSubpasses = spss,
	M.createInfoDependencies = dps }
