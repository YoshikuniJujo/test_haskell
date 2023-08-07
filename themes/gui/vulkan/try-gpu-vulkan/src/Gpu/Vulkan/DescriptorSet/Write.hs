{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet.Write (

	-- * WRITE

	-- ** Types

	Write(..), WriteSources(..), WriteSourcesArg(..),

	-- ** WriteListToMiddle

	WriteListToMiddle(..), WriteSourcesToMiddle,
	BindingAndArrayElemImage, BindingAndArrayElemImageWithImmutableSampler,
	BindingAndArrayElemBuffer, BindingAndArrayElemBufferView,

	-- ** WriteListUpdateDynamicLengths

	WriteListUpdateDynamicLengths(..),
	WriteSourcesUpdateDynamicLengths, UpdateDynamicLength

	) where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Data.TypeLevel.Tuple.Index qualified as TIndex
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.Type

import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import Gpu.Vulkan.DescriptorSet.Write.Sources

-- * WRITE

data Write mn sds slbts wsarg (i :: Nat) = Write {
	writeNext :: TMaybe.M mn,
	writeDstSet :: D sds slbts,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources wsarg }

-- ** WriteListToMiddle

class M.WriteListToCore (TMapIndex.M0_5 wargs) => WriteListToMiddle wargs where
	writeListToMiddle ::
		HeteroParList.PL (U5 Write) wargs ->
		HeteroParList.PL M.Write (TMapIndex.M0_5 wargs)

instance WriteListToMiddle '[] where
	writeListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked (TMaybe.M mn),
	WriteSourcesToMiddle (TIndex.I1_2 slbts) wsarg i,
	WriteListToMiddle wargs ) =>
	WriteListToMiddle ('(mn, sds, slbts, wsarg, i) ': wargs) where
	writeListToMiddle (U5 w :** ws) =
		writeToMiddle w :** writeListToMiddle ws

writeToMiddle :: forall mn s slbts wsa i .
	WriteSourcesToMiddle (TIndex.I1_2 slbts) wsa i =>
	Write mn s slbts wsa i -> M.Write mn
writeToMiddle Write {
	writeNext = mnxt,
	writeDstSet = D _ ds,
	writeDescriptorType = dt,
	writeSources = srcs } = M.Write {
		M.writeNext = mnxt,
		M.writeDstSet = ds,
		M.writeDstBinding = bdg, M.writeDstArrayElement = ae,
		M.writeDescriptorType = dt,
		M.writeSources = srcs' }
	where ((bdg, ae), srcs') =
		writeSourcesToMiddle @(TIndex.I1_2 slbts) @_ @i srcs

-- ** WriteListUpdateDynamicLengths

class WriteListUpdateDynamicLengths wargs where
	writeListUpdateDynamicLength ::
		HeteroParList.PL (U5 Write) wargs -> IO ()

instance WriteListUpdateDynamicLengths '[] where
	writeListUpdateDynamicLength HeteroParList.Nil = pure ()

instance (
	WriteSourcesUpdateDynamicLengths bts wsarg,
	WriteListUpdateDynamicLengths wargs ) =>
	WriteListUpdateDynamicLengths
		('(mn, sds, '(sl, bts), wsarg, i) ': wargs) where
	writeListUpdateDynamicLength (U5 w :** ws) =
		writeUpdateDynamicLength w >> writeListUpdateDynamicLength ws

writeUpdateDynamicLength ::
	WriteSourcesUpdateDynamicLengths bts wsarg =>
	Write n s '(sl, bts) wsarg i -> IO ()
writeUpdateDynamicLength Write { writeDstSet = ds, writeSources = ws } =
	writeSourcesUpdateDynamicLength ds ws
