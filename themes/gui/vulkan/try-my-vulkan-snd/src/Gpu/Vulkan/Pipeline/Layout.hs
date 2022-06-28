{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.Layout (
	L, create, CreateInfo(..),
	create'', CreateInfo''(..),
	M.CreateFlags, pattern M.CreateFlagsZero, Layout(..) ) where

import Foreign.Pointable
import Control.Exception
import Data.Kind
import Data.HeteroList

import Gpu.Vulkan.Pipeline.Layout.Type

import qualified Gpu.Vulkan.AllocationCallbacks as AllocationCallbacks
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Descriptor.Set.Layout
import qualified Gpu.Vulkan.PushConstant as PushConstant
import qualified Gpu.Vulkan.Pipeline.Layout.Middle as M

data CreateInfo n sbtss = CreateInfo {
	createInfoNext :: Maybe n,
	createInfoFlags :: M.CreateFlags,
	createInfoSetLayouts :: HeteroVarList Layout sbtss,
	createInfoPushConstantRanges :: [PushConstant.Range] }

data CreateInfo'' n ss sbtss = CreateInfo'' {
	createInfoNext'' :: Maybe n,
	createInfoFlags'' :: M.CreateFlags,
	createInfoSetLayouts'' :: Either
		(HeteroVarList Descriptor.Set.Layout.L'' ss)
		(HeteroVarList Layout sbtss),
	createInfoPushConstantRanges'' :: [PushConstant.Range] }

createInfoToCreateInfo :: CreateInfo n sbtss -> CreateInfo'' n '[] sbtss
createInfoToCreateInfo CreateInfo {
	createInfoNext = mnxt,
	createInfoFlags = flgs,
	createInfoSetLayouts = lyts,
	createInfoPushConstantRanges = pcrs } = CreateInfo'' {
	createInfoNext'' = mnxt,
	createInfoFlags'' = flgs,
	createInfoSetLayouts'' = Right lyts,
	createInfoPushConstantRanges'' = pcrs }

deriving instance (
	Show n,
	Show (HeteroVarList Descriptor.Set.Layout.L'' ss),
	Show (HeteroVarList Layout sbtss) ) =>
	Show (CreateInfo'' n ss sbtss)

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

createInfoToMiddle :: HeteroVarListToList' sbtss =>
	CreateInfo'' n ss (sbtss :: [(Type, [Descriptor.Set.Layout.BindingType])]) -> M.CreateInfo n
createInfoToMiddle CreateInfo'' {
	createInfoNext'' = mnxt,
	createInfoFlags'' = flgs,
	createInfoSetLayouts'' = either
		(heteroVarListToList Descriptor.Set.Layout.unL'')
		(heteroVarListToList' $ Descriptor.Set.Layout.unL . unLayout) ->
		sls,
	createInfoPushConstantRanges'' = pcrs
	} = M.CreateInfo {
		M.createInfoNext = mnxt,
		M.createInfoFlags = flgs,
		M.createInfoSetLayouts = sls,
		M.createInfoPushConstantRanges = pcrs }

create :: (Pointable n, Pointable c, Pointable d, HeteroVarListToList' sbtss) =>
	Device.D sd -> CreateInfo n sbtss ->
	Maybe (AllocationCallbacks.A c) -> Maybe (AllocationCallbacks.A d) ->
	(forall s . L s -> IO a) -> IO a
create dvc (createInfoToCreateInfo -> ci) = create'' dvc ci

create'' :: (Pointable n, Pointable n2, Pointable n3, HeteroVarListToList' sbtss) =>
	Device.D sd -> CreateInfo'' n ss sbtss ->
	Maybe (AllocationCallbacks.A n2) -> Maybe (AllocationCallbacks.A n3) ->
	(forall s . L s -> IO a) -> IO a
create'' (Device.D dvc) (createInfoToMiddle -> ci) macc macd f =
	bracket (M.create dvc ci macc) (\l -> M.destroy dvc l macd) (f . L)
