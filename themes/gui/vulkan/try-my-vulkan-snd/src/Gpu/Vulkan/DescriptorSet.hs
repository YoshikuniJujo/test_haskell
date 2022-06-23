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

import qualified Gpu.Vulkan.Device.Type as Device
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

data Layout slbts where
	Layout :: Layout.L' sl bts -> Layout '(sl, bts)

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

newtype S' sd sp (slbts :: (Type, [Layout.BindingType])) = S' M.S

allocateSs' :: (Pointable n, ListToHeteroVarList slbtss) =>
	Device.D sd -> AllocateInfo' n sp slbtss ->
	IO (HeteroVarList (S' sd sp) slbtss)
allocateSs' (Device.D dvc) ai =
	listToHeteroVarList S' <$> M.allocateSs dvc (allocateInfoToMiddle' ai)

-- data Write n sd sp slbts
