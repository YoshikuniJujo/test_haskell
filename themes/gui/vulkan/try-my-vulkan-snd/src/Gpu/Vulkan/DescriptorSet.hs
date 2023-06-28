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

module Gpu.Vulkan.DescriptorSet (

	-- * ALLOCATE

	allocateDs, D, AllocateInfo(..), DListFromMiddle, DefaultDynamicLengths,

	-- * UPDATE

	updateDs,
	Write(..), WriteListToMiddle,
	Copy(..), CopyListToMiddle

	) where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.Default
import Data.Word
import Data.IORef
import Data.Kind.Object qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))
import qualified Data.HeteroParList.Tuple as HeteroParList

import Gpu.Vulkan.Buffer.Type qualified as Buffer
import Gpu.Vulkan.DescriptorSet.Type
import Gpu.Vulkan.DescriptorSet.TypeLevel.Write
import Gpu.Vulkan.DescriptorSet.TypeLevel.Copy qualified as Copy

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Descriptor.Internal as Descriptor
import qualified Gpu.Vulkan.Descriptor.Enum as Descriptor
import qualified Gpu.Vulkan.DescriptorPool.Type as Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as Layout.M
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import Gpu.Vulkan.Misc

type Layout = U2 Layout.L

layoutToMiddle :: Layout slbts -> Layout.M.L
layoutToMiddle (U2 (Layout.L l)) = l

data AllocateInfo mn sp slbtss = AllocateInfo {
	allocateInfoNext :: TMaybe.M mn,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts :: HeteroParList.PL (U2 Layout.L) slbtss }

deriving instance (Show (TMaybe.M n), Show (HeteroParList.PL Layout slbtss)) =>
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
			HeteroParList.toList layoutToMiddle dscsls }

class DListFromMiddle slbtss where
	dListFromMiddle :: [M.D] -> IO (HeteroParList.PL (D s) slbtss)

instance DListFromMiddle '[] where
	dListFromMiddle = \case [] -> pure HeteroParList.Nil; _ -> error "bad"

instance (
	DefaultDynamicLengths slbts,
	DListFromMiddle slbtss ) =>
	DListFromMiddle (slbts ': slbtss) where
	dListFromMiddle = \case
		(d : ds) -> (:**)
			<$> ((`D` d) <$> newDefaultIORef)
			<*> dListFromMiddle @slbtss ds
		_ -> error "bad"

type DefaultDynamicLengths slbts = Default
	(HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(LayoutArgOnlyDynamics slbts))

allocateDs :: (WithPoked (TMaybe.M mn), DListFromMiddle slbtss) =>
	Device.D sd -> AllocateInfo mn sp slbtss ->
	(forall s . HeteroParList.PL (D s) slbtss -> IO a) -> IO a
allocateDs (Device.D dvc) ai f = do
	dsm <- M.allocateDs dvc (allocateInfoToMiddle ai)
	ds <- dListFromMiddle dsm
	f ds <* M.freeDs dvc
		((\(Descriptor.Pool.P p) -> p) $ allocateInfoDescriptorPool ai)
		dsm

data Write n s (slbts :: LayoutArg)
	(sbsmobjsobjs :: WriteSourcesArg) = Write {
	writeNextNew :: TMaybe.M n,
	writeDstSetNew :: D s slbts,
	writeDescriptorTypeNew :: Descriptor.Type,
	writeSourcesNew :: WriteSources sbsmobjsobjs }

data Copy n sdss (slbtss :: LayoutArg) sdsd (slbtsd :: LayoutArg)
	(bts :: Layout.BindingType) (is :: Nat) (id :: Nat) = Copy {
	copyNextNew :: TMaybe.M n,
	copySrcSetNew :: D sdss slbtss,
	copyDstSetNew :: D sdsd slbtsd }

class M.CopyListToCore (CopyNextsNew copyArgs) =>
	CopyListToMiddle copyArgs where
	type CopyNextsNew copyArgs :: [Maybe Type]
	copyListToMiddleNew ::
		HeteroParList.PL (U8 Copy) copyArgs ->
		HeteroParList.PL M.Copy (CopyNextsNew copyArgs)

instance CopyListToMiddle '[] where
	type CopyNextsNew '[] = '[]
	copyListToMiddleNew HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked (TMaybe.M n),
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id, Copy.BindingLength bts,
	CopyListToMiddle copyArgs ) =>
	CopyListToMiddle (
		'(n, sdss, '(sls, btss), sdsd, '(sld, btsd), bts, is, id) ':
		copyArgs) where
	type CopyNextsNew (
		'(n, sdss, '(sls, btss), sdsd, '(sld, btsd), bts, is, id) ':
		copyArgs ) = n ': CopyNextsNew copyArgs
	copyListToMiddleNew (U8 c :** cs) = copyToMiddleNew c :** copyListToMiddleNew cs

copyToMiddleNew :: (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id, Copy.BindingLength bts ) =>
	Copy n sdss '(sls, btss) sdsd '(sld, btsd) bts is id -> M.Copy n
copyToMiddleNew c@Copy {
	copyNextNew = mnxt, copySrcSetNew = D _ ss, copyDstSetNew = D _ ds } = let
	(sb, sae, db, dae, cnt) = getCopyArgsNew c in
	M.Copy {
		M.copyNext = mnxt,
		M.copySrcSet = ss,
		M.copySrcBinding = sb,
		M.copySrcArrayElement = sae, M.copyDstSet = ds,
		M.copyDstBinding = db,
		M.copyDstArrayElement = dae, M.copyDescriptorCount = cnt }

getCopyArgsNew :: forall n sdss sls btss sdsd sld btsd bts is id . (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id,
	Copy.BindingLength bts ) =>
	Copy n sdss '(sls, btss) sdsd '(sld, btsd) bts is id ->
	(Word32, Word32, Word32, Word32, Word32)
getCopyArgsNew _ = let
	(sb, sae) = Copy.bindingAndArrayElement @btss @bts @is
	(db, dae) = Copy.bindingAndArrayElement @btsd @bts @id in
	(sb, sae, db, dae, Copy.bindingLength @bts)

writeUpdateLengthNew :: forall sbsmobjsobjs n s sl bts . (
	WriteSourcesToLengthList sbsmobjsobjs,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj sbsmobjsobjs) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj sbsmobjsobjs)
	) =>
	Write n s '(sl, bts) sbsmobjsobjs -> IO ()
writeUpdateLengthNew Write {
	writeDstSetNew = D rlns _,
	writeSourcesNew = ws } = do
	lns <- readIORef rlns
	maybe	(pure ())
		(writeIORef rlns . updateDynamicLength @bts @(WriteSourcesToLengthListObj sbsmobjsobjs) @0 lns
			. (VObj.onlyDynamicLength @(WriteSourcesToLengthListObj sbsmobjsobjs)))
		(writeSourcesToLengthList @sbsmobjsobjs ws)

class M.WriteListToCore (WriteNextsNew nsdspslbtswsas) => WriteListToMiddle nsdspslbtswsas where
	type WriteNextsNew nsdspslbtswsas :: [Maybe Type]
	writeListToMiddleNewNew ::
		HeteroParList.PL (U4 Write) nsdspslbtswsas ->
		HeteroParList.PL M.Write (WriteNextsNew nsdspslbtswsas)
	writeListUpdateLengthNewNew ::
		HeteroParList.PL (U4 Write) nsdspslbtswsas -> IO ()

instance WriteListToMiddle '[] where
	type WriteNextsNew '[] = '[]
	writeListToMiddleNewNew HeteroParList.Nil = HeteroParList.Nil
	writeListUpdateLengthNewNew HeteroParList.Nil = pure ()

instance (
	WithPoked (TMaybe.M n),
	WriteSourcesToMiddle '(sl, bts) wsa,
	WriteListToMiddle nsdspslbtswsas,
	WriteSourcesToLengthList wsa,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj wsa) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj wsa) ) =>
	WriteListToMiddle ('(n, s, '(sl, bts), wsa) ': nsdspslbtswsas) where
	type WriteNextsNew ('(n, s, '(sl, bts), wsa) ': nsdspslbtswsas) =
		n ': WriteNextsNew nsdspslbtswsas
	writeListToMiddleNewNew (U4 w :** ws) =
		writeToMiddleNew w :** writeListToMiddleNewNew ws
	writeListUpdateLengthNewNew (U4 w :** ws) =
		writeUpdateLengthNew w >> writeListUpdateLengthNewNew ws

writeToMiddleNew :: forall n s slbts wsa . WriteSourcesToMiddle slbts wsa =>
	Write n s slbts wsa -> M.Write n
writeToMiddleNew Write {
	writeNextNew = mnxt,
	writeDstSetNew = D _ ds,
	writeDescriptorTypeNew = dt,
	writeSourcesNew = srcs
	} = M.Write {
		M.writeNext = mnxt,
		M.writeDstSet = ds,
		M.writeDstBinding = bdg,
		M.writeDstArrayElement = ae,
		M.writeDescriptorType = dt,
		M.writeSources = srcs' }
	where ((bdg, ae), srcs') = writeSourcesToMiddle @slbts srcs

class WriteSourcesToLengthList arg where
	type WriteSourcesToLengthListObj arg :: [VObj.Object]
	writeSourcesToLengthList :: WriteSources arg ->
		Maybe (HeteroParList.PL
			VObj.ObjectLength (WriteSourcesToLengthListObj arg))

instance
	HeteroParList.Map3_4 sbsmobjsobjs =>
	WriteSourcesToLengthList ('WriteSourcesArgBufferNew sbsmobjsobjs) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgBufferNew sbsmobjsobjs) =
		TMapIndex.M3_4 sbsmobjsobjs
	writeSourcesToLengthList (BufferInfosNew bis) =
		Just $ bufferInfoListToLengthNew bis

bufferInfoListToLengthNew :: HeteroParList.Map3_4 sbsmobjsobjs =>
	HeteroParList.PL (U4 Descriptor.BufferInfo) sbsmobjsobjs ->
	HeteroParList.PL VObj.ObjectLength (TMapIndex.M3_4 sbsmobjsobjs)
bufferInfoListToLengthNew = HeteroParList.map3_4 $ bufferInfoToLengthNew . unU4

bufferInfoToLengthNew :: Descriptor.BufferInfo sb sm nm obj -> VObj.ObjectLength obj
bufferInfoToLengthNew (Descriptor.BufferInfo (Buffer.Binded lns _)) = HeteroParList.typeIndex lns

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

updateDs :: (WriteListToMiddle writeArgs, CopyListToMiddle copyArgs) =>
	Device.D sd ->
	HeteroParList.PL (U4 Write) writeArgs ->
	HeteroParList.PL (U8 Copy) copyArgs  -> IO ()
updateDs (Device.D dvc) ws cs =
	writeListUpdateLengthNewNew ws >> M.updateDs dvc ws' cs'
	where ws' = writeListToMiddleNewNew ws; cs' = copyListToMiddleNew cs
