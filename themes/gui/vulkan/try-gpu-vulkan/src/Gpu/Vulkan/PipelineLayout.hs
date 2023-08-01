{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineLayout (

	-- * CREATE

	create, P, CreateInfo(..), M.CreateFlags

	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList

import Gpu.Vulkan.PipelineLayout.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DscStLyt
import qualified Gpu.Vulkan.PushConstant.Internal as PushConstant
import qualified Gpu.Vulkan.PipelineLayout.Middle as M

-- CREATE

create :: (
	WithPoked (TMaybe.M mn),
	HeteroParList.ToListT2 Type [DscStLyt.BindingType] lytas,
	PushConstant.RangeListToMiddle whole ranges,
	AllocationCallbacks.ToMiddle mac ) =>
	Device.D sd ->
	CreateInfo mn lytas ('PushConstant.Layout whole ranges) ->
	TPMaybe.M (U2 AllocationCallbacks.A) mac ->
	(forall s . P s lytas whole -> IO a) -> IO a
create (Device.D dvc) (createInfoToMiddle -> ci)
	(AllocationCallbacks.toMiddle -> mac) f =
	bracket (M.create dvc ci mac) (\l -> M.destroy dvc l mac) (f . P)

-- CREATE INFO

data CreateInfo mn lytas (pcl :: PushConstant.Layout) = CreateInfo {
	createInfoNext :: TMaybe.M mn, createInfoFlags :: M.CreateFlags,
	createInfoSetLayouts :: HeteroParList.PL (U2 DscStLyt.D) lytas }

deriving instance (
	Show (TMaybe.M mn), Show (HeteroParList.PL (U2 DscStLyt.D) lytas) ) =>
	Show (CreateInfo mn lytas pcl)

createInfoToMiddle :: forall n k lytas whole ranges . (
	PushConstant.RangeListToMiddle whole ranges,
	HeteroParList.ToListT2 k [DscStLyt.BindingType] lytas ) =>
	CreateInfo n lytas ('PushConstant.Layout whole ranges) -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts = HeteroParList.toListT2
		$ DscStLyt.unL . unU2 -> sls } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoSetLayouts = sls,
	M.createInfoPushConstantRanges =
		PushConstant.rangeListToMiddle @whole @ranges }
