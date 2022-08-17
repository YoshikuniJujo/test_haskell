{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout (
	LL, create, CreateInfo(..),
	L,
	M.CreateFlags, pattern M.CreateFlagsZero, Layout(..) ) where

import Foreign.Pointable
import Control.Exception
import Data.Kind
import Data.HeteroList

import Gpu.Vulkan.Pipeline.Layout.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Descriptor.Set.Layout
import qualified Gpu.Vulkan.PushConstant.Middle as PushConstant
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as M

data CreateInfo n sbtss = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlags,
	createInfoSetLayouts :: HeteroVarList Layout sbtss,
	createInfoPushConstantRanges :: [PushConstant.Range] }

deriving instance (
	Show n,
	Show (HeteroVarList Layout sbtss) ) =>
	Show (CreateInfo n sbtss)

data Layout sbts where
	Layout :: Descriptor.Set.Layout.L s bts -> Layout '(s, bts)

unLayout :: Layout '(s, bts) -> Descriptor.Set.Layout.L s bts
unLayout (Layout l) = l

class HeteroVarListToList' sbtss where
	heteroVarListToList' ::
		(forall (s :: Type) (bts :: [Descriptor.Set.Layout.BindingType]) . t '(s, bts) -> t') ->
		HeteroVarList t sbtss -> [t']

instance HeteroVarListToList' '[] where heteroVarListToList' _ HVNil = []

instance HeteroVarListToList' sbtss => HeteroVarListToList' ('(s, bts) ': sbtss) where
	heteroVarListToList' f (x :...: xs) = f x : heteroVarListToList' f xs

createInfoToMiddle ::
	HeteroVarListToList' sbtss => CreateInfo n sbtss -> M.CreateInfo n
createInfoToMiddle CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts = heteroVarListToList'
		$ Descriptor.Set.Layout.unL . unLayout -> sls,
	createInfoPushConstantRanges = pcrs } = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoSetLayouts = sls,
		M.createInfoPushConstantRanges = pcrs }

create :: (Pointable n, Pointable c, Pointable d, HeteroVarListToList' sbtss) =>
	Device.D sd -> CreateInfo n sbtss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . LL s sbtss -> IO a) -> IO a
create (Device.D dvc) (createInfoToMiddle -> ci) macc macd f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . LL)
