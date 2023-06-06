{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Tmp where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Uncurry
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.RenderPass.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Attachment as Attachment
import qualified Gpu.Vulkan.Subpass.Middle as Subpass

import Gpu.Vulkan.RenderPass.Middle

data CreateInfoNew mn fmts = CreateInfoNew {
	createInfoNextNew :: TMaybe.M mn,
	createInfoFlagsNew :: CreateFlags,
	createInfoAttachmentsNew ::
		HeteroParList.PL Attachment.Description fmts,
	createInfoSubpassesNew :: [Subpass.Description],
	createInfoDependenciesNew :: [Subpass.Dependency] }

createInfoFromNew :: Attachment.DescriptionListToMiddle fmts =>
	CreateInfoNew n fmts -> CreateInfo n
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoAttachmentsNew = atts,
	createInfoSubpassesNew = spss,
	createInfoDependenciesNew = dps } = CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoAttachments = Attachment.descriptionListToMiddle atts,
	createInfoSubpasses = spss,
	createInfoDependencies = dps }

createNew :: (
	WithPoked (TMaybe.M mn), Attachment.DescriptionListToMiddle fmts,
	AllocationCallbacks.ToMiddle msc ) =>
	Device.D -> CreateInfoNew mn fmts ->
	TPMaybe.M (U2 AllocationCallbacks.A) msc -> IO R
createNew dvc ci mac =
	create dvc (createInfoFromNew ci) (AllocationCallbacks.toMiddle mac)
