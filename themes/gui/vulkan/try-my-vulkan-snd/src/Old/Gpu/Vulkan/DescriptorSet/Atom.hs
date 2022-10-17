{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Gpu.Vulkan.DescriptorSet.Atom where

import Foreign.Pointable
import Data.Word

import qualified Foreign.Storable.Generic

import qualified Gpu.Vulkan.Device.Middle as Device
import qualified Gpu.Vulkan.BufferView.Middle as Buffer.View
import qualified Gpu.Vulkan.Descriptor.Enum as Dsc

import qualified Gpu.Vulkan.Descriptor.Middle as M

import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import qualified Old.Gpu.Vulkan.Descriptor.Atom as Dsc

data Write n v = Write {
	writeNext :: Maybe n,
	writeDstSet :: M.D,
	writeDstBinding :: Word32,
	writeDstArrayElement :: Word32,
	writeDescriptorType :: Dsc.Type,
	writeImageBufferInfoTexelBufferViews ::
		Either Word32 (ImageBufferInfoTexelBufferViews v) }
	deriving Show

data ImageBufferInfoTexelBufferViews v
	= ImageInfos [M.ImageInfo]
	| BufferInfos [Dsc.BufferInfo v]
	| TexelBufferViews [Buffer.View.B]
	deriving Show

writeSourcesToMiddle :: Foreign.Storable.Generic.G v =>
	ImageBufferInfoTexelBufferViews v -> M.WriteSources
writeSourcesToMiddle = \case
	ImageInfos iis -> M.WriteSourcesImageInfo iis
	BufferInfos ((Dsc.bufferInfoToMiddle <$>) -> bis) ->
		M.WriteSourcesBufferInfo bis
	TexelBufferViews bvs -> M.WriteSourcesBufferView bvs

writeToMiddle :: Foreign.Storable.Generic.G v => Write n v -> M.Write n
writeToMiddle Write {
	writeNext = mnxt,
	writeDstSet = s,
	writeDstBinding = bdg,
	writeDstArrayElement = ae,
	writeDescriptorType = tp,
	writeImageBufferInfoTexelBufferViews =
		either M.WriteSourcesInNext writeSourcesToMiddle -> srcs } =
	M.Write {
		M.writeNext = mnxt,
		M.writeDstSet = s,
		M.writeDstBinding = bdg,
		M.writeDstArrayElement = ae,
		M.writeDescriptorType = tp,
		M.writeSources = srcs }

updateDs :: (Pointable n, Pointable n', Foreign.Storable.Generic.G v) =>
	Device.D -> [Write n v] -> [M.Copy n'] -> IO ()
updateDs dvc ((writeToMiddle <$>) -> ws) cs = M.updateDs dvc ws cs
