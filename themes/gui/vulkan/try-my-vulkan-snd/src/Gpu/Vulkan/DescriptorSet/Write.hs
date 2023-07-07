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

	Write(..), WriteListToMiddle(..), WriteListUpdateDynamicLengths(..),
	WriteSources(..), WriteSourcesArg(..), WriteSourcesToMiddle,

	WriteSourcesUpdateDynamicLengths,
	BufferInfoListToMiddle,
	BufferViewListToMiddle

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

data Write mn sds slbts writeSourcesArg (i :: Nat) = Write {
	writeNext :: TMaybe.M mn,
	writeDstSet :: D sds slbts,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources writeSourcesArg }

class M.WriteListToCore (TMapIndex.M0_5 writeArgs) =>
	WriteListToMiddle writeArgs where
	writeListToMiddle ::
		HeteroParList.PL (U5 Write) writeArgs ->
		HeteroParList.PL M.Write (TMapIndex.M0_5 writeArgs)

instance WriteListToMiddle '[] where
	writeListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked (TMaybe.M mn), WriteSourcesToMiddle (TIndex.I1_2 slbts) writeSourcesArg i,
	WriteListToMiddle writeArgs ) =>
	WriteListToMiddle
		('(mn, sds, slbts, writeSourcesArg, i) ': writeArgs) where
	writeListToMiddle (U5 w :** ws) =
		writeToMiddle w :** writeListToMiddle ws

writeToMiddle :: forall n s slbts wsa i . WriteSourcesToMiddle (TIndex.I1_2 slbts) wsa i =>
	Write n s slbts wsa i -> M.Write n
writeToMiddle Write {
	writeNext = mnxt,
	writeDstSet = D _ ds,
	writeDescriptorType = dt,
	writeSources = srcs
	} = M.Write {
		M.writeNext = mnxt,
		M.writeDstSet = ds,
		M.writeDstBinding = bdg,
		M.writeDstArrayElement = ae,
		M.writeDescriptorType = dt,
		M.writeSources = srcs' }
	where ((bdg, ae), srcs') = writeSourcesToMiddle @(TIndex.I1_2 slbts) @_ @i srcs

class WriteListUpdateDynamicLengths writeArgs where
	writeListUpdateDynamicLength ::
		HeteroParList.PL (U5 Write) writeArgs -> IO ()

instance WriteListUpdateDynamicLengths '[] where
	writeListUpdateDynamicLength HeteroParList.Nil = pure ()

instance (
	WriteSourcesUpdateDynamicLengths bts writeSourcesArg,
	WriteListUpdateDynamicLengths writeArgs ) =>
	WriteListUpdateDynamicLengths
		('(mn, sds, '(sl, bts), writeSourcesArg, i) ': writeArgs) where
	writeListUpdateDynamicLength (U5 w :** ws) =
		writeUpdateDynamicLength w >> writeListUpdateDynamicLength ws

writeUpdateDynamicLength ::
	WriteSourcesUpdateDynamicLengths bts sbsmobjsobjs =>
	Write n s '(sl, bts) sbsmobjsobjs i -> IO ()
writeUpdateDynamicLength Write { writeDstSet = ds, writeSources = ws } =
	writeSourcesUpdateDynamicLength ds ws
