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

	Write(..), WriteListToMiddle(..), WriteSources(..), WriteSourcesArg(..)

	) where

import Foreign.Storable.PeekPoke
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.IORef
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import qualified Data.HeteroParList.Tuple as HeteroParList

import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.DescriptorSet.Type
import Gpu.Vulkan.DescriptorSet.TypeLevel.Common

import qualified Gpu.Vulkan.Descriptor.Internal as Descriptor
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
	writeListUpdateLength ::
		HeteroParList.PL (U4 Write) writeArgs -> IO ()

instance WriteListToMiddle '[] where
	writeListToMiddle HeteroParList.Nil = HeteroParList.Nil
	writeListUpdateLength HeteroParList.Nil = pure ()

instance (
	WithPoked (TMaybe.M n),
	WriteSourcesToMiddle '(sl, bts) wsa,
	WriteListToMiddle writeArgs,
	WriteSourcesToLengthList wsa,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj wsa) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj wsa) ) =>
	WriteListToMiddle ('(n, s, '(sl, bts), wsa) ': writeArgs) where
	writeListToMiddle (U4 w :** ws) =
		writeToMiddle w :** writeListToMiddle ws
	writeListUpdateLength (U4 w :** ws) =
		writeUpdateLength w >> writeListUpdateLength ws

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

writeUpdateLength :: forall sbsmobjsobjs n s sl bts . (
	WriteSourcesToLengthList sbsmobjsobjs,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj sbsmobjsobjs) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj sbsmobjsobjs)
	) =>
	Write n s '(sl, bts) sbsmobjsobjs -> IO ()
writeUpdateLength Write {
	writeDstSet = D rlns _,
	writeSources = ws } = do
	lns <- readIORef rlns
	maybe	(pure ())
		(writeIORef rlns . updateDynamicLength @bts @(WriteSourcesToLengthListObj sbsmobjsobjs) @0 lns
			. (VObj.onlyDynamicLength @(WriteSourcesToLengthListObj sbsmobjsobjs)))
		(writeSourcesToLengthList @sbsmobjsobjs ws)

class WriteSourcesToLengthList arg where
	type WriteSourcesToLengthListObj arg :: [VObj.Object]
	writeSourcesToLengthList :: WriteSources arg ->
		Maybe (HeteroParList.PL
			VObj.ObjectLength (WriteSourcesToLengthListObj arg))

instance
	HeteroParList.Map3_4 sbsmobjsobjs =>
	WriteSourcesToLengthList ('WriteSourcesArgBuffer sbsmobjsobjs) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgBuffer sbsmobjsobjs) =
		TMapIndex.M3_4 sbsmobjsobjs
	writeSourcesToLengthList (BufferInfos bis) =
		Just $ bufferInfoListToLength bis

bufferInfoListToLength :: HeteroParList.Map3_4 sbsmobjsobjs =>
	HeteroParList.PL (U4 Descriptor.BufferInfo) sbsmobjsobjs ->
	HeteroParList.PL VObj.ObjectLength (TMapIndex.M3_4 sbsmobjsobjs)
bufferInfoListToLength = HeteroParList.map3_4 $ bufferInfoToLength . unU4

bufferInfoToLength :: Descriptor.BufferInfo sb sm nm obj -> VObj.ObjectLength obj
bufferInfoToLength (Descriptor.BufferInfo (Buffer.Binded lns _)) = HeteroParList.typeIndex lns

instance WriteSourcesToLengthList ('WriteSourcesArgImage ssfmtnmsis) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgImage ssfmtnmsis) = '[]
	writeSourcesToLengthList (ImageInfos _bis) = Nothing

instance WriteSourcesToLengthList ('WriteSourcesArgBufferView foo) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgBufferView foo) = '[]
	writeSourcesToLengthList (TexelBufferViews _) = Nothing

instance WriteSourcesToLengthList 'WriteSourcesArgInNext where
	type WriteSourcesToLengthListObj 'WriteSourcesArgInNext = '[]
	writeSourcesToLengthList (WriteSourcesInNext _ _ _) = Nothing
