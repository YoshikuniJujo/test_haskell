{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.RenderPass (

	-- * CRATE

	create, R, CreateInfo(..),

	-- * BEGIN INFO

	BeginInfo(..)

	) where

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

create :: (
	Attachment.DescriptionListToMiddle fmts, WithPoked (TMaybe.M mn),
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo mn fmts ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . R s -> IO a) -> IO a
create (Device.D dvc) ci (AllocationCallbacks.toMiddle -> macd) f = bracket(M.create dvc (createInfoFrom ci) macd)
		(\r -> M.destroy dvc r macd) (f . R)

data CreateInfo mn fmts = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: CreateFlags,
	createInfoAttachments ::
		HeteroParList.PL Attachment.Description fmts,
	createInfoSubpasses :: [Subpass.Description],
	createInfoDependencies :: [Subpass.Dependency] }

createInfoFrom :: Attachment.DescriptionListToMiddle fmts =>
	CreateInfo n fmts -> M.CreateInfo n
createInfoFrom CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoAttachments = atts,
	createInfoSubpasses = spss,
	createInfoDependencies = dps } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoAttachments = Attachment.descriptionListToMiddle atts,
	M.createInfoSubpasses = spss,
	M.createInfoDependencies = dps }
