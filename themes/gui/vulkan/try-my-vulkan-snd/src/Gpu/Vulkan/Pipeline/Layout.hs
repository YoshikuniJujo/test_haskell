{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout (
	L, createNew, CreateInfoNew(..),
	M.CreateFlags,
	Layout
	) where

import Foreign.Storable.PeekPoke
import Control.Exception
import Data.Kind
import Data.TypeLevel
import Data.HeteroParList

import Gpu.Vulkan.Pipeline.Layout.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Descriptor.Set.Layout
import qualified Gpu.Vulkan.PushConstant as PushConstant
import qualified Gpu.Vulkan.PushConstant.Middle as PushConstant.M
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as M

data CreateInfo n sbtss = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlags,
	createInfoSetLayouts :: HeteroParList Layout sbtss,
	createInfoPushConstantRanges :: [PushConstant.M.Range] }

data CreateInfoNew n sbtss (pcl :: PushConstant.PushConstantLayout) = CreateInfoNew {
	createInfoNextNew :: Maybe n,
	createInfoFlagsNew :: M.CreateFlags,
	createInfoSetLayoutsNew :: HeteroParList Layout sbtss }

deriving instance (
	Show n,
	Show (HeteroParList Layout sbtss) ) =>
	Show (CreateInfo n sbtss)

type Layout = V2 Descriptor.Set.Layout.L

unLayout :: Layout '(s, bts) -> Descriptor.Set.Layout.L s bts
unLayout (V2 l) = l

class HeteroParListToList' sbtss where
	heteroParListToList' ::
		(forall (s :: Type) (bts :: [Descriptor.Set.Layout.BindingType]) . t '(s, bts) -> t') ->
		HeteroParList t sbtss -> [t']

instance HeteroParListToList' '[] where heteroParListToList' _ HNil = []

instance HeteroParListToList' sbtss => HeteroParListToList' ('(s, bts) ': sbtss) where
	heteroParListToList' f (x :** xs) = f x : heteroParListToList' f xs

createInfoToMiddle ::
	HeteroParListToList' sbtss => CreateInfo n sbtss -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts = heteroParListToList'
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
	Pokable n, Pokable c, Pokable d, HeteroParListToList' sbtss ) =>
	Device.D sd -> CreateInfoNew n sbtss pcl ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . L s sbtss whole -> IO a) -> IO a
createNew (Device.D dvc) (createInfoToMiddleNew -> ci) macc macd f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . L)
