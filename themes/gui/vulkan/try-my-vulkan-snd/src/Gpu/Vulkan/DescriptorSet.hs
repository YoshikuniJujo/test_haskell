{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet where

import Foreign.Pointable
import Data.Kind
import Data.HeteroList
import Data.Word

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.BufferView.Middle as BufferView.M
import qualified Gpu.Vulkan.Descriptor as Descriptor
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.Descriptor.Middle as Descriptor.M
import qualified Gpu.Vulkan.DescriptorPool.Type as Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as Layout.M
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

data AllocateInfo n sp sl = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts :: [Layout.L sl] }
	deriving Show

allocateInfoToMiddle :: AllocateInfo n sp sl -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Descriptor.Pool.P dp,
	allocateInfoSetLayouts = (Layout.unL <$>) -> dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoSetLayouts = dscsls }

newtype S sd sp sl = S M.S deriving Show

allocateSs :: Pointable n =>
	Device.D sd -> AllocateInfo n sp sl -> IO [S sd sp sl]
allocateSs (Device.D dvc) ai = (S <$>) <$> M.allocateSs dvc (allocateInfoToMiddle ai)

data Layout (slbts :: LayoutArg) where
	Layout :: Layout.L' sl bts -> Layout '(sl, bts)

type LayoutArg = (Type, [Layout.BindingType])

layoutToMiddle :: Layout slbts -> Layout.M.L
layoutToMiddle (Layout (Layout.L' l)) = l

data AllocateInfo' n sp slbtss = AllocateInfo' {
	allocateInfoNext' :: Maybe n,
	allocateInfoDescriptorPool' :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts' :: HeteroVarList Layout slbtss }

deriving instance (Show n, Show (HeteroVarList Layout slbtss)) =>
	Show (AllocateInfo' n sp slbtss)

allocateInfoToMiddle' :: AllocateInfo' n sp slbtss -> M.AllocateInfo n
allocateInfoToMiddle' AllocateInfo' {
	allocateInfoNext' = mnxt,
	allocateInfoDescriptorPool' = Descriptor.Pool.P dp,
	allocateInfoSetLayouts' = dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoSetLayouts =
			heteroVarListToList layoutToMiddle dscsls }

newtype S' sd sp (slbts :: LayoutArg) = S' M.S

allocateSs' :: (Pointable n, ListToHeteroVarList slbtss) =>
	Device.D sd -> AllocateInfo' n sp slbtss ->
	IO (HeteroVarList (S' sd sp) slbtss)
allocateSs' (Device.D dvc) ai =
	listToHeteroVarList S' <$> M.allocateSs dvc (allocateInfoToMiddle' ai)

data Write n sd sp (slbts :: LayoutArg)
	(sbsmobjsobjs :: [Descriptor.BufferInfoArg]) = Write {
	writeNext :: Maybe n,
	writeDstSet :: S' sd sp slbts,
	writeDescriptorType :: Descriptor.Type,
	writeImageBufferInfoTexelBufferViews ::
		Either Word32 (ImageBufferInfoTexelBufferViews sbsmobjsobjs) }

deriving instance (
	Show n, Show (S' sd sp slbts),
	Show (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs)) =>
	Show (Write n sd sp slbts sbsmobjsobjs)

data ImageBufferInfoTexelBufferViews sbsmobjsobjs
	= ImageInfos [Descriptor.M.ImageInfo]
	| BufferInfos (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs)
	| TexelBufferViews [BufferView.M.B]

deriving instance
	Show (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs) =>
	Show (ImageBufferInfoTexelBufferViews sbsmobjsobjs)
