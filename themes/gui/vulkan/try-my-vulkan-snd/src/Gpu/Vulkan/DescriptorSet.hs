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

import GHC.TypeLits
import Foreign.Pointable
import Data.Kind
import Data.Kind.Object
import Data.HeteroList
import Data.Word

import Gpu.Vulkan.DescriptorSet.TypeLevel

import qualified Gpu.Vulkan.TypeEnum as T
import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.BufferView.Middle as BufferView.M
import qualified Gpu.Vulkan.Descriptor as Descriptor
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.Descriptor.Middle as Descriptor.M
import qualified Gpu.Vulkan.DescriptorPool.Type as Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as Layout.M
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

data AllocateInfo'' n sp sl = AllocateInfo'' {
	allocateInfoNext'' :: Maybe n,
	allocateInfoDescriptorPool'' :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts'' :: [Layout.L'' sl] }
	deriving Show

allocateInfoToMiddle'' :: AllocateInfo'' n sp sl -> M.AllocateInfo n
allocateInfoToMiddle'' AllocateInfo'' {
	allocateInfoNext'' = mnxt,
	allocateInfoDescriptorPool'' = Descriptor.Pool.P dp,
	allocateInfoSetLayouts'' = (Layout.unL'' <$>) -> dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoSetLayouts = dscsls }

newtype S'' sd sp sl = S'' M.D deriving Show

allocateSs'' :: Pointable n =>
	Device.D sd -> AllocateInfo'' n sp sl -> IO [S'' sd sp sl]
allocateSs'' (Device.D dvc) ai = (S'' <$>) <$> M.allocateDs dvc (allocateInfoToMiddle'' ai)

data Layout (slbts :: LayoutArg) where
	Layout :: Layout.L sl bts -> Layout '(sl, bts)

type LayoutArg = (Type, [Layout.BindingType])

layoutToMiddle :: Layout slbts -> Layout.M.L
layoutToMiddle (Layout (Layout.L l)) = l

data AllocateInfo n sp slbtss = AllocateInfo {
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts :: HeteroVarList Layout slbtss }

deriving instance (Show n, Show (HeteroVarList Layout slbtss)) =>
	Show (AllocateInfo n sp slbtss)

allocateInfoToMiddle :: AllocateInfo n sp slbtss -> M.AllocateInfo n
allocateInfoToMiddle AllocateInfo {
	allocateInfoNext = mnxt,
	allocateInfoDescriptorPool = Descriptor.Pool.P dp,
	allocateInfoSetLayouts = dscsls
	} = M.AllocateInfo {
		M.allocateInfoNext = mnxt,
		M.allocateInfoDescriptorPool = dp,
		M.allocateInfoSetLayouts =
			heteroVarListToList layoutToMiddle dscsls }

newtype S sd sp (slbts :: LayoutArg) = S M.D

allocateSs :: (Pointable n, ListToHeteroVarList slbtss) =>
	Device.D sd -> AllocateInfo n sp slbtss ->
	IO (HeteroVarList (S sd sp) slbtss)
allocateSs (Device.D dvc) ai =
	listToHeteroVarList S <$> M.allocateDs dvc (allocateInfoToMiddle ai)

data Write n sd sp (slbts :: LayoutArg)
	(sbsmobjsobjs :: WriteSourcesArg) = Write {
	writeNext :: Maybe n,
	writeDstSet :: S sd sp slbts,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources sbsmobjsobjs }

deriving instance (
	Show n, Show (S sd sp slbts),
	Show (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs)) =>
	Show (Write n sd sp slbts ('WriteSourcesArgBuffer sbsmobjsobjs))

writeToMiddle :: forall n sd sp slbts wsa . WriteSourcesToMiddle slbts wsa =>
	Write n sd sp slbts wsa -> M.Write n
writeToMiddle Write {
	writeNext = mnxt,
	writeDstSet = S ds,
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

data WriteSourcesArg
	= WriteSourcesArgImage [(Type, T.Format, Symbol, Type)]
	| WriteSourcesArgBuffer [Descriptor.BufferInfoArg]
	| WriteSourcesArgOther

data WriteSources arg where
	WriteSourcesInNext ::
		Word32 -> Word32 -> Word32 -> WriteSources 'WriteSourcesArgOther
	ImageInfos ::
		HeteroVarList (V4 Descriptor.ImageInfo) ssfmtnmsis ->
		WriteSources ('WriteSourcesArgImage ssfmtnmsis)
	BufferInfos ::
		HeteroVarList Descriptor.BufferInfo sbsmobjsobjs ->
		WriteSources ('WriteSourcesArgBuffer sbsmobjsobjs)
	TexelBufferViews ::
		Word32 -> Word32 -> [BufferView.M.B] ->
		WriteSources 'WriteSourcesArgOther

deriving instance Show (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs) =>
	Show (WriteSources ('WriteSourcesArgBuffer sbsmobjsobjs))

class WriteSourcesToMiddle (slbts :: LayoutArg) wsarg where
	writeSourcesToMiddle ::
		WriteSources wsarg -> ((Word32, Word32), M.WriteSources)

instance (
	BindingAndArrayElem
		(BindingTypesFromLayoutArg slbts)
		(ObjectsFromBufferInfoArgs sbsmobjsobjs),
	BufferInfosToMiddle sbsmobjsobjs ) =>
	WriteSourcesToMiddle slbts ('WriteSourcesArgBuffer sbsmobjsobjs) where
	writeSourcesToMiddle (BufferInfos bis) = (
		bindingAndArrayElem' @slbts @sbsmobjsobjs,
		M.WriteSourcesBufferInfo $ bufferInfosToMiddle bis )

instance (
	BindingAndArrayElemImage bts ssfmtnmsis,
	ImageInfosToMiddle ssfmtnmsis ) =>
	WriteSourcesToMiddle '(sl, bts) ('WriteSourcesArgImage ssfmtnmsis) where
	writeSourcesToMiddle (ImageInfos iis) = (
		bindingAndArrayElemImage @bts @ssfmtnmsis 0 0,
		M.WriteSourcesImageInfo $ imageInfosToMiddle iis )

instance WriteSourcesToMiddle slbts 'WriteSourcesArgOther where
	writeSourcesToMiddle = \case
		WriteSourcesInNext bdg ae cnt -> ((bdg, ae), M.WriteSourcesInNext cnt)
		TexelBufferViews bdg ae bvs -> ((bdg, ae), M.WriteSourcesBufferView bvs)

class ImageInfosToMiddle ssfmtnmsis where
	imageInfosToMiddle ::
		HeteroVarList (V4 Descriptor.ImageInfo) ssfmtnmsis ->
		[Descriptor.M.ImageInfo]

instance ImageInfosToMiddle '[] where imageInfosToMiddle HVNil = []

instance ImageInfosToMiddle ssfmtnmsis =>
	ImageInfosToMiddle ('(ss, fmt, nm, si) ': ssfmtnmsis) where
	imageInfosToMiddle (V4 ii :...: iis) =
		Descriptor.imageInfoToMiddle ii : imageInfosToMiddle iis

class BufferInfosToMiddle sbsmobjsobjs where
	bufferInfosToMiddle ::
		HeteroVarList Descriptor.BufferInfo sbsmobjsobjs ->
		[Descriptor.M.BufferInfo]

instance BufferInfosToMiddle '[] where bufferInfosToMiddle HVNil = []

instance (Offset obj objs, BufferInfosToMiddle sbsmobjsobjs) =>
	BufferInfosToMiddle ('(sb, sm, nm, objs, obj) ': sbsmobjsobjs) where
	bufferInfosToMiddle (bi :...: bis) =
		Descriptor.bufferInfoToMiddle bi : bufferInfosToMiddle bis

data Write_ n sdspslbtssbsmobjsobjs where
	Write_ :: Write n sd sp slbts sbsmobjsobjs ->
		Write_ n '(sd, sp, slbts, sbsmobjsobjs)

deriving instance (
	Show n, Show (S sd sp slbts),
	Show (HeteroVarList Descriptor.BufferInfo sbsmobjsobjs) ) =>
	Show (Write_ n '(sd, sp, slbts, ('WriteSourcesArgBuffer sbsmobjsobjs)))

class WriteListToMiddle n sdspslbtssbsmobjsobjs where
	writeListToMiddle ::
		HeteroVarList (Write_ n) sdspslbtssbsmobjsobjs -> [M.Write n]

instance WriteListToMiddle n '[] where writeListToMiddle HVNil = []

instance (
	WriteSourcesToMiddle slbts wsa,
	WriteListToMiddle n sdspslbtswsas ) =>
	WriteListToMiddle n
		('(sd, sp, slbts, wsa) ': sdspslbtswsas) where
	writeListToMiddle (Write_ w :...: ws) =
		writeToMiddle w : writeListToMiddle ws

updateDs :: (
	Pointable n, Pointable n',
	WriteListToMiddle n sdspslbtssbsmobjsobjs ) =>
	Device.D sd ->
	HeteroVarList (Write_ n) sdspslbtssbsmobjsobjs -> [M.Copy n'] -> IO ()
updateDs (Device.D dvc) (writeListToMiddle -> ws) cs = M.updateDs dvc ws cs
