{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds, GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Gpu.Vulkan.DescriptorSet.List where

import Foreign.Pointable
import Data.HeteroList
import Data.Word

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.BufferView.Middle as Buffer.View
import qualified Gpu.Vulkan.Descriptor.Enum as Dsc
import qualified Gpu.Vulkan.Descriptor.Middle as Dsc.M
import qualified Gpu.Vulkan.DescriptorSet as Dsc.Set
import qualified Gpu.Vulkan.DescriptorSet.Middle as Dsc.Set.M
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import qualified Old.Gpu.Vulkan.Descriptor.List as Dsc

data Write n sd sp sl slsmvs = Write {
	writeNext :: Maybe n,
	writeDstSet :: Dsc.Set.S'' sd sp sl,
	writeDstBinding :: Word32,
	writeDstArrayElement :: Word32,
	writeDescriptorType :: Dsc.Type,
	writeImageBufferInfoTexelBufferViews ::
		Either Word32 (ImageBufferInfoTexelBufferViews slsmvs) }

deriving instance (Show n, Show (HeteroVarList Dsc.BufferInfo slsmvs)) =>
	Show (Write n sd sp sl slsmvs)

data ImageBufferInfoTexelBufferViews slsmvs
	= ImageInfos [Dsc.M.ImageInfo]
	| BufferInfos (HeteroVarList Dsc.BufferInfo slsmvs)
	| TexelBufferViews [Buffer.View.B]

writeSourcesToMiddle :: Dsc.BufferInfoListToMiddle slsmvs =>
	ImageBufferInfoTexelBufferViews slsmvs -> M.WriteSources
writeSourcesToMiddle = \case
	ImageInfos iis -> M.WriteSourcesImageInfo iis
	BufferInfos (Dsc.bufferInfoListToMiddle -> bis) ->
		M.WriteSourcesBufferInfo bis
	TexelBufferViews tbvs -> M.WriteSourcesBufferView tbvs

deriving instance Show (HeteroVarList Dsc.BufferInfo slsmvs) =>
	Show (ImageBufferInfoTexelBufferViews slsmvs)

data Write_ n sdspslslsmvs where
	Write_ :: Write n sd sp sl slsmvs -> Write_ n '(sd, sp, sl, slsmvs)

writeToMiddle :: Dsc.BufferInfoListToMiddle slsmvs =>
	Write n sd sp sl slsmvs -> M.Write n
writeToMiddle Write {
	writeNext = mnxt,
	writeDstSet = Dsc.Set.S'' s,
	writeDstBinding = bdg,
	writeDstArrayElement = ae,
	writeDescriptorType = tp,
	writeImageBufferInfoTexelBufferViews = either
		M.WriteSourcesInNext writeSourcesToMiddle -> srcs } = M.Write {
	M.writeNext = mnxt,
	M.writeDstSet = s,
	M.writeDstBinding = bdg,
	M.writeDstArrayElement = ae,
	M.writeDescriptorType = tp,
	M.writeSources = srcs }

class WriteListToMiddle n sdspslslsmvs where
	writeListToMiddle ::
		HeteroVarList (Write_ n) sdspslslsmvs -> [M.Write n]

instance WriteListToMiddle n '[] where writeListToMiddle HVNil = []

instance (
	Pointable n,
	Dsc.BufferInfoListToMiddle slsmvs, WriteListToMiddle n sdspslslsmvs ) =>
	WriteListToMiddle n ('(sd, sp, sl, slsmvs) ': sdspslslsmvs) where
	writeListToMiddle (Write_ w :...: ws) =
		writeToMiddle w : writeListToMiddle ws

updateDs :: (WriteListToMiddle n sdspslslsmvs, Pointable n, Pointable n') =>
	Device.D sd -> HeteroVarList (Write_ n) sdspslslsmvs ->
	[Dsc.Set.M.Copy n'] -> IO ()
updateDs (Device.D dvc) (writeListToMiddle -> ws) cs = M.updateDs dvc ws cs
