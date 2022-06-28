{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet where

import Foreign.Pointable
import Data.Kind
import Data.Kind.Object
import Data.HeteroList
import Data.Word

import Gpu.Vulkan.DescriptorSet.TypeLevel

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
	allocateInfoSetLayouts :: [Layout.L'' sl] }
	deriving Show

allocateInfoToMiddle :: AllocateInfo n sp sl -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Descriptor.Pool.P dp,
	allocateInfoSetLayouts = (Layout.unL'' <$>) -> dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoSetLayouts = dscsls }

newtype S sd sp sl = S M.S deriving Show

allocateSs :: Pointable n =>
	Device.D sd -> AllocateInfo n sp sl -> IO [S sd sp sl]
allocateSs (Device.D dvc) ai = (S <$>) <$> M.allocateSs dvc (allocateInfoToMiddle ai)

data Layout (slbts :: LayoutArg) where
	Layout :: Layout.L sl bts -> Layout '(sl, bts)

type LayoutArg = (Type, [Layout.BindingType])

layoutToMiddle :: Layout slbts -> Layout.M.L
layoutToMiddle (Layout (Layout.L l)) = l

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
	writeSources :: WriteSources sbsmobjsobjs }

deriving instance (
	Show n, Show (S' sd sp slbts),
	Show (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs)) =>
	Show (Write n sd sp slbts sbsmobjsobjs)

writeToMiddle :: forall n sd sp slbts sbsmobjsobjs . (
	BufferInfosToMiddle sbsmobjsobjs,
	BindingAndArrayElem
		(BindingTypesFromLayoutArg slbts)
		(ObjectsFromBufferInfoArgs sbsmobjsobjs) ) =>
	Write n sd sp slbts sbsmobjsobjs -> M.Write n
writeToMiddle Write {
	writeNext = mnxt,
	writeDstSet = S' ds,
	writeDescriptorType = dt,
	writeSources = srcs
	} = M.Write {
		M.writeNext = mnxt,
		M.writeDstSet = ds,
		M.writeDstBinding = bdg,
		M.writeDstArrayElement = ae,
		M.writeDescriptorType = dt,
		M.writeSources = srcs' }
	where ((bdg, ae), srcs') = writeSourcesToMiddle @slbts srcs

data WriteSources sbsmobjsobjs
	= WriteSourcesInNext Word32 Word32 Word32
	| ImageInfos Word32 Word32 [Descriptor.M.ImageInfo]
	| BufferInfos (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs)
	| TexelBufferViews Word32 Word32 [BufferView.M.B]

deriving instance Show (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs) =>
	Show (WriteSources sbsmobjsobjs)

writeSourcesToMiddle :: forall (slbts :: LayoutArg) sbsmobjsobjs . (
	BindingAndArrayElem
		(BindingTypesFromLayoutArg slbts)
		(ObjectsFromBufferInfoArgs sbsmobjsobjs),
	BufferInfosToMiddle sbsmobjsobjs ) =>
	WriteSources sbsmobjsobjs -> ((Word32, Word32), M.WriteSources)
writeSourcesToMiddle = \case
	WriteSourcesInNext bdg ae cnt -> ((bdg, ae), M.WriteSourcesInNext cnt)
	ImageInfos bdg ae iis -> ((bdg, ae), M.WriteSourcesImageInfo iis)
	BufferInfos bis -> (
		bindingAndArrayElem' @slbts @sbsmobjsobjs,
		M.WriteSourcesBufferInfo $ bufferInfosToMiddle bis )
	TexelBufferViews bdg ae bvs -> ((bdg, ae), M.WriteSourcesBufferView bvs)

class BufferInfosToMiddle sbsmobjsobjs where
	bufferInfosToMiddle ::
		HeteroVarList Descriptor.BufferInfo sbsmobjsobjs ->
		[Descriptor.M.BufferInfo]

instance BufferInfosToMiddle '[] where bufferInfosToMiddle HVNil = []

instance (Offset obj objs, BufferInfosToMiddle sbsmobjsobjs) =>
	BufferInfosToMiddle ('(sb, sm, objs, obj) ': sbsmobjsobjs) where
	bufferInfosToMiddle (bi :...: bis) =
		Descriptor.bufferInfoToMiddle bi : bufferInfosToMiddle bis

data Write_ n sdspslbtssbsmobjsobjs where
	Write_ :: Write n sd sp slbts sbsmobjsobjs ->
		Write_ n '(sd, sp, slbts, sbsmobjsobjs)

deriving instance (
	Show n, Show (S' sd sp slbts),
	Show (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs) ) =>
	Show (Write_ n '(sd, sp, slbts, sbsmobjsobjs))

class WriteListToMiddle n sdspslbtssbsmobjsobjs where
	writeListToMiddle ::
		HeteroVarList (Write_ n) sdspslbtssbsmobjsobjs -> [M.Write n]

instance WriteListToMiddle n '[] where writeListToMiddle HVNil = []

instance (
	BufferInfosToMiddle sbsmobjsobjs,
	BindingAndArrayElem
		(BindingTypesFromLayoutArg slbts)
		(ObjectsFromBufferInfoArgs sbsmobjsobjs),
	WriteListToMiddle n sdspslbtssbsmobjsobjs ) =>
	WriteListToMiddle n
		('(sd, sp, slbts, sbsmobjsobjs) ': sdspslbtssbsmobjsobjs) where
	writeListToMiddle (Write_ w :...: ws) =
		writeToMiddle w : writeListToMiddle ws

updateDs :: (
	Pointable n, Pointable n',
	WriteListToMiddle n sdspslbtssbsmobjsobjs ) =>
	Device.D sd ->
	HeteroVarList (Write_ n) sdspslbtssbsmobjsobjs -> [M.Copy n'] -> IO ()
updateDs (Device.D dvc) (writeListToMiddle -> ws) cs = M.updateDs dvc ws cs
