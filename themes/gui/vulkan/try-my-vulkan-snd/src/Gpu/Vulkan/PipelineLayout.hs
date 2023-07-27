{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.PipelineLayout (
	P, createNew, CreateInfoNew(..),
	M.CreateFlags,
	Layout
	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe qualified as TPMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.PipelineLayout.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Descriptor.Set.Layout
import qualified Gpu.Vulkan.PushConstant as PushConstant
import qualified Gpu.Vulkan.PushConstant.Middle as PushConstant.M
import qualified Gpu.Vulkan.PipelineLayout.Middle as M

data CreateInfo mn sbtss = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: M.CreateFlags,
	createInfoSetLayouts :: HeteroParList.PL Layout sbtss,
	createInfoPushConstantRanges :: [PushConstant.M.Range] }

data CreateInfoNew mn sbtss (pcl :: PushConstant.PushConstantLayout) = CreateInfoNew {
	createInfoNextNew :: TMaybe.M mn,
	createInfoFlagsNew :: M.CreateFlags,
	createInfoSetLayoutsNew :: HeteroParList.PL Layout sbtss }

deriving instance (
	Show (TMaybe.M mn),
	Show (HeteroParList.PL Layout sbtss) ) =>
	Show (CreateInfo mn sbtss)

type Layout = U2 Descriptor.Set.Layout.L

unLayout :: Layout '(s, bts) -> Descriptor.Set.Layout.L s bts
unLayout (U2 l) = l

class HeteroParListToList' sbtss where
	toList' ::
		(forall (s :: Type) (bts :: [Descriptor.Set.Layout.BindingType]) . t '(s, bts) -> t') ->
		HeteroParList.PL t sbtss -> [t']

instance HeteroParListToList' '[] where toList' _ HeteroParList.Nil = []

instance HeteroParListToList' sbtss => HeteroParListToList' ('(s, bts) ': sbtss) where
	toList' f (x :** xs) = f x : toList' f xs

createInfoToMiddle ::
	HeteroParListToList' sbtss => CreateInfo n sbtss -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts = toList'
		$ Descriptor.Set.Layout.unL . unLayout -> sls,
	createInfoPushConstantRanges = pcrs } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoSetLayouts = sls,
		M.createInfoPushConstantRanges = pcrs }

createInfoFromNew ::
	forall n sbtss pcl whole ranges .
	(	pcl ~ ('PushConstant.PushConstantLayout whole ranges),
		PushConstant.RangesToMiddle whole ranges ) =>
	CreateInfoNew n sbtss pcl -> CreateInfo n sbtss
createInfoFromNew CreateInfoNew {
	createInfoNextNew = mnxt,
	createInfoFlagsNew = flgs,
	createInfoSetLayoutsNew = slyt } = CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts = slyt,
	createInfoPushConstantRanges =
		PushConstant.pushConstantLayoutToRanges @pcl }

createInfoToMiddleNew :: (
	pcl ~ ('PushConstant.PushConstantLayout whole ranges),
	PushConstant.RangesToMiddle whole ranges,
	HeteroParListToList' sbtss ) =>
	CreateInfoNew n sbtss pcl -> M.CreateInfo n
createInfoToMiddleNew = createInfoToMiddle . createInfoFromNew

createNew :: (
	pcl ~ ('PushConstant.PushConstantLayout whole ranges),
	PushConstant.RangesToMiddle whole ranges,
	WithPoked (TMaybe.M mn), HeteroParListToList' sbtss,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfoNew mn sbtss pcl ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . P s sbtss whole -> IO a) -> IO a
createNew (Device.D dvc) (createInfoToMiddleNew -> ci)
	(AllocationCallbacks.toMiddle -> macc) f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macc) (f . P)
