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

	WriteSourcesUpdateDynamicLengths

	) where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.IORef
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Tuple.Index qualified as TIndex
import qualified Data.HeteroParList as HeteroParList
import qualified Data.HeteroParList.Tuple as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.Type
import Gpu.Vulkan.DescriptorSet.UpdateDynamicLengths

import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import Gpu.Vulkan.DescriptorSet.Write.Sources

data Write mn sds slbts writeSourcesArg = Write {
	writeNext :: TMaybe.M mn,
	writeDstSet :: D sds slbts,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources writeSourcesArg }

class M.WriteListToCore (TMapIndex.M0_4 writeArgs) =>
	WriteListToMiddle writeArgs where
	writeListToMiddle ::
		HeteroParList.PL (U4 Write) writeArgs ->
		HeteroParList.PL M.Write (TMapIndex.M0_4 writeArgs)

instance WriteListToMiddle '[] where
	writeListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked (TMaybe.M mn), WriteSourcesToMiddle slbts writeSourcesArg,
	WriteListToMiddle writeArgs ) =>
	WriteListToMiddle
		('(mn, sds, slbts, writeSourcesArg) ': writeArgs) where
	writeListToMiddle (U4 w :** ws) =
		writeToMiddle w :** writeListToMiddle ws

writeToMiddle :: forall n s slbts wsa . WriteSourcesToMiddle slbts wsa =>
	Write n s slbts wsa -> M.Write n
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
	where ((bdg, ae), srcs') = writeSourcesToMiddle @slbts srcs

class WriteListUpdateDynamicLengths writeArgs where
	writeListUpdateDynamicLength ::
		HeteroParList.PL (U4 Write) writeArgs -> IO ()

instance WriteListUpdateDynamicLengths '[] where
	writeListUpdateDynamicLength HeteroParList.Nil = pure ()

instance (
	WriteListUpdateDynamicLengths writeArgs,

	WriteSourcesUpdateDynamicLengths slbts writeSourcesArg
	) =>
	WriteListUpdateDynamicLengths
		('(mn, sds, slbts, writeSourcesArg) ': writeArgs) where
	writeListUpdateDynamicLength (U4 w :** ws) =
		writeUpdateDynamicLength w >> writeListUpdateDynamicLength ws

writeUpdateDynamicLength ::
	WriteSourcesUpdateDynamicLengths slbts sbsmobjsobjs =>
	Write n s slbts sbsmobjsobjs -> IO ()
writeUpdateDynamicLength Write { writeDstSet = ds, writeSources = ws } =
	writeSourcesUpdateDynamicLength ds ws

class WriteSourcesUpdateDynamicLengths slbts arg where
	writeSourcesUpdateDynamicLength :: D sds slbts -> WriteSources arg -> IO ()

instance (
	UpdateDynamicLength (TIndex.I1_2 slbts) (TMapIndex.M3_4 foo),
	HeteroParList.Map3_4 foo ) =>
	WriteSourcesUpdateDynamicLengths slbts (WriteSourcesArgBuffer foo) where
	writeSourcesUpdateDynamicLength (D rlns _) (BufferInfos bis) = do
		lns <- readIORef rlns
		(writeIORef rlns . updateDynamicLength @(TIndex.I1_2 slbts) @(TMapIndex.M3_4 foo) lns
			. (VObj.onlyDynamicLength @(TMapIndex.M3_4 foo)))
			(writeSourcesToLengthListBuffer @foo bis)

instance {-# OVERLAPPABLE #-} WriteSourcesUpdateDynamicLengths slbts foo where
	writeSourcesUpdateDynamicLength _ _ = pure ()
