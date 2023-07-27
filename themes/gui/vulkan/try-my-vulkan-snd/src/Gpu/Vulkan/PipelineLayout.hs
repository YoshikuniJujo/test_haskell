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
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.PipelineLayout.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.AllocationCallbacks.Type as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DscStLyt
import qualified Gpu.Vulkan.PushConstant as PushConstant
import qualified Gpu.Vulkan.PipelineLayout.Middle as M

data CreateInfo mn sbtss (pcl :: PushConstant.PushConstantLayout) = CreateInfo {
	createInfoNext :: TMaybe.M mn,
	createInfoFlags :: M.CreateFlags,
	createInfoSetLayouts :: HeteroParList.PL (U2 DscStLyt.L) sbtss }

deriving instance (
	Show (TMaybe.M mn),
	Show (HeteroParList.PL (U2 DscStLyt.L) sbtss) ) =>
	Show (CreateInfo mn sbtss pcl)

class HeteroParListToList' sbtss where
	toList' ::
		(forall (s :: Type) (bts :: [DscStLyt.BindingType]) . t '(s, bts) -> t') ->
		HeteroParList.PL t sbtss -> [t']

instance HeteroParListToList' '[] where toList' _ HeteroParList.Nil = []

instance HeteroParListToList' sbtss => HeteroParListToList' ('(s, bts) ': sbtss) where
	toList' f (x :** xs) = f x : toList' f xs

createInfoToMiddleNew' ::
	forall n sbtss pcl whole ranges .
	(	pcl ~ ('PushConstant.PushConstantLayout whole ranges),
		PushConstant.RangesToMiddle whole ranges ) =>
	HeteroParListToList' sbtss => CreateInfo n sbtss pcl -> M.CreateInfo n
createInfoToMiddleNew' CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts = toList'
		$ DscStLyt.unL . unU2 -> sls } = M.CreateInfo {
	M.createInfoNext = mnxt,
	M.createInfoFlags = flgs,
	M.createInfoSetLayouts = sls,
	M.createInfoPushConstantRanges =
		PushConstant.pushConstantLayoutToRanges @pcl }

create :: (
	pcl ~ ('PushConstant.PushConstantLayout whole ranges),
	PushConstant.RangesToMiddle whole ranges,
	WithPoked (TMaybe.M mn), HeteroParListToList' sbtss,
	AllocationCallbacks.ToMiddle mscc ) =>
	Device.D sd -> CreateInfo mn sbtss pcl ->
	TPMaybe.M (U2 AllocationCallbacks.A) mscc ->
	(forall s . P s sbtss whole -> IO a) -> IO a
create (Device.D dvc) (createInfoToMiddleNew' -> ci)
	(AllocationCallbacks.toMiddle -> macc) f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macc) (f . P)
