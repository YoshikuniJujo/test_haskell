{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Descriptor.Set.List where

import Data.HeteroList
import Data.Word

import qualified Vulkan.Buffer.View as Buffer.View
import qualified Vulkan.Descriptor.List as Dsc
import qualified Vulkan.Descriptor.Enum as Dsc
import qualified Vulkan.Descriptor.Middle as Dsc.M
import qualified Vulkan.Descriptor.Set as Dsc.Set

data Write n sd sp sl slsmvs = Write {
	writeNext :: Maybe n,
	writeDstSet :: Dsc.Set.S sd sp sl,
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
	| TexelBufferViews [Buffer.View.V]

deriving instance Show (HeteroVarList Dsc.BufferInfo slsmvs) => Show (ImageBufferInfoTexelBufferViews slsmvs)
