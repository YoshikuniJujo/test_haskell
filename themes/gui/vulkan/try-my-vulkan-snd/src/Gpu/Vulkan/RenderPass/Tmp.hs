{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass.Tmp where

import Foreign.Storable.PeekPoke
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:*), pattern (:**))

import Gpu.Vulkan.RenderPass.Enum

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.Attachment as Attachment
import qualified Gpu.Vulkan.Subpass.Middle as Subpass

import Gpu.Vulkan.RenderPass.Middle

data CreateInfoNew n fmts = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: CreateFlags,
	createInfoAttachmentsNew ::
		HeteroParList.PL Attachment.DescriptionNew fmts,
	createInfoSubpassesNew :: [Subpass.Description],
	createInfoDependenciesNew :: [Subpass.Dependency] }

createInfoFromNew :: Attachment.DescriptionsFromNew fmts =>
	CreateInfoNew n fmts -> CreateInfo n
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoAttachmentsNew = atts,
	createInfoSubpassesNew = spss,
	createInfoDependenciesNew = dps } = CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoAttachments = Attachment.descriptionsFromNew atts,
	createInfoSubpasses = spss,
	createInfoDependencies = dps }

createNew ::
	(Pokable n, Pokable c, Attachment.DescriptionsFromNew fmts) =>
	Device.D ->
	CreateInfoNew n fmts -> Maybe (AllocationCallbacks.A c) -> IO R
createNew dvc ci mac = create dvc (createInfoFromNew ci) mac
