{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.DescriptorSet where

import GHC.TypeLits
import Foreign.Storable.PeekPoke
import Data.Kind
import Data.Default
import Data.Word
import Data.IORef
import Data.Kind.Object qualified as KObj
import Gpu.Vulkan.Object qualified as VObj
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.Tuple.Uncurry
import qualified Data.HeteroParList as HeteroParList
import Data.HeteroParList (pattern (:**))

import Gpu.Vulkan.DescriptorSet.TypeLevel.Write
import Gpu.Vulkan.DescriptorSet.TypeLevel.Copy qualified as Copy

import qualified Gpu.Vulkan.Device.Type as Device
import qualified Gpu.Vulkan.Descriptor as Descriptor
import qualified Gpu.Vulkan.DescriptorPool.Type as Descriptor.Pool
import qualified Gpu.Vulkan.DescriptorSetLayout.Type as Layout
import qualified Gpu.Vulkan.DescriptorSetLayout.Middle as Layout.M
import qualified Gpu.Vulkan.DescriptorSet.Middle as M

import Gpu.Vulkan.Misc

type Layout = U2 Layout.L

layoutToMiddle :: Layout slbts -> Layout.M.L
layoutToMiddle (U2 (Layout.L l)) = l

data AllocateInfo n sp slbtss = AllocateInfo {
	allocateInfoNext :: TMaybe.M n,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts :: HeteroParList.PL Layout slbtss }

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

data S sd sp (slbts :: LayoutArg) = S
	(IORef (HeteroParList.PL2 KObj.ObjectLength
		(LayoutArgOnlyDynamics slbts)))
	M.D

data SNew s (slbts :: LayoutArg) = SNew
	(IORef (HeteroParList.PL2 KObj.ObjectLength
		(LayoutArgOnlyDynamics slbts)))
	M.D

class SListFromMiddle slbtss where
	sListFromMiddle :: [M.D] -> IO (HeteroParList.PL (S sd sp) slbtss)

instance SListFromMiddle '[] where
	sListFromMiddle = \case [] -> pure HeteroParList.Nil; _ -> error "bad"

instance (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(LayoutArgOnlyDynamics slbts)),
	SListFromMiddle slbtss ) =>
	SListFromMiddle (slbts ': slbtss) where
	sListFromMiddle = \case
		(d : ds) -> (:**)
			<$> ((`S` d) <$> newDefaultIORef)
			<*> sListFromMiddle @slbtss ds
		_ -> error "bad"

class SListFromMiddleNew slbtss where
	sListFromMiddleNew :: [M.D] -> IO (HeteroParList.PL (SNew s) slbtss)

instance SListFromMiddleNew '[] where
	sListFromMiddleNew = \case [] -> pure HeteroParList.Nil; _ -> error "bad"

instance (
	Default (HeteroParList.PL
		(HeteroParList.PL KObj.ObjectLength)
		(LayoutArgOnlyDynamics slbts)),
	SListFromMiddleNew slbtss ) =>
	SListFromMiddleNew (slbts ': slbtss) where
	sListFromMiddleNew = \case
		(d : ds) -> (:**)
			<$> ((`SNew` d) <$> newDefaultIORef)
			<*> sListFromMiddleNew @slbtss ds
		_ -> error "bad"

allocateSs :: (WithPoked (TMaybe.M n), SListFromMiddle slbtss) =>
	Device.D sd -> AllocateInfo n sp slbtss ->
	IO (HeteroParList.PL (S sd sp) slbtss)
allocateSs (Device.D dvc) ai =
	sListFromMiddle =<< M.allocateDs dvc (allocateInfoToMiddle ai)

allocateSsNew :: (WithPoked (TMaybe.M n), SListFromMiddleNew slbtss) =>
	Device.D sd -> AllocateInfo n sp slbtss ->
	(forall s . HeteroParList.PL (SNew s) slbtss -> IO a) -> IO a
allocateSsNew (Device.D dvc) ai f = do
	dsm <- M.allocateDs dvc (allocateInfoToMiddle ai)
	ds <- sListFromMiddleNew dsm
	f ds <* M.freeDs dvc
		((\(Descriptor.Pool.P p) -> p) $ allocateInfoDescriptorPool ai)
		dsm

data Write n sd sp (slbts :: LayoutArg)
	(sbsmobjsobjs :: WriteSourcesArg) = Write {
	writeNext :: TMaybe.M n,
	writeDstSet :: S sd sp slbts,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources sbsmobjsobjs }

data WriteNew n s (slbts :: LayoutArg)
	(sbsmobjsobjs :: WriteSourcesArg) = WriteNew {
	writeNextNew :: TMaybe.M n,
	writeDstSetNew :: SNew s slbts,
	writeDescriptorTypeNew :: Descriptor.Type,
	writeSourcesNew :: WriteSources sbsmobjsobjs }

data Copy n sds sps (slbtss :: LayoutArg) sdd spd (slbtsd :: LayoutArg)
	(bts :: Layout.BindingType) (is :: Nat) (id :: Nat) = Copy {
	copyNext :: TMaybe.M n,
	copySrcSet :: S sds sps slbtss,
	copyDstSet :: S sdd spd slbtsd }

data CopyNew n sdss (slbtss :: LayoutArg) sdsd (slbtsd :: LayoutArg)
	(bts :: Layout.BindingType) (is :: Nat) (id :: Nat) = CopyNew {
	copyNextNew :: TMaybe.M n,
	copySrcSetNew :: SNew sdss slbtss,
	copyDstSetNew :: SNew sdsd slbtsd }

class CopyListToMiddle copyArgs where
	type CopyNexts copyArgs :: [Maybe Type]
	copyListToMiddle ::
		HeteroParList.PL (U10 Copy) copyArgs ->
		HeteroParList.PL M.Copy (CopyNexts copyArgs)

instance CopyListToMiddle '[] where
	type CopyNexts '[] = '[]
	copyListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id, Copy.BindingLength bts,
	CopyListToMiddle copyArgs ) =>
	CopyListToMiddle (
		'(n, sds, sps, '(sls, btss), sdd, spd, '(sld, btsd), bts, is, id) ':
		copyArgs) where
	type CopyNexts (
		'(n, sds, sps, '(sls, btss), sdd, spd, '(sld, btsd), bts, is, id) ':
		copyArgs ) = n ': CopyNexts copyArgs
	copyListToMiddle (U10 c :** cs) = copyToMiddle c :** copyListToMiddle cs

class CopyListToMiddleNew copyArgs where
	type CopyNextsNew copyArgs :: [Maybe Type]
	copyListToMiddleNew ::
		HeteroParList.PL (U8 CopyNew) copyArgs ->
		HeteroParList.PL M.Copy (CopyNextsNew copyArgs)

instance CopyListToMiddleNew '[] where
	type CopyNextsNew '[] = '[]
	copyListToMiddleNew HeteroParList.Nil = HeteroParList.Nil

instance (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id, Copy.BindingLength bts,
	CopyListToMiddleNew copyArgs ) =>
	CopyListToMiddleNew (
		'(n, sdss, '(sls, btss), sdsd, '(sld, btsd), bts, is, id) ':
		copyArgs) where
	type CopyNextsNew (
		'(n, sdss, '(sls, btss), sdsd, '(sld, btsd), bts, is, id) ':
		copyArgs ) = n ': CopyNextsNew copyArgs
	copyListToMiddleNew (U8 c :** cs) = copyToMiddleNew c :** copyListToMiddleNew cs

copyToMiddle :: (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id, Copy.BindingLength bts ) =>
	Copy n sds sps '(sls, btss) sdd spd '(sld, btsd) bts is id -> M.Copy n
copyToMiddle c@Copy {
	copyNext = mnxt, copySrcSet = S _ ss, copyDstSet = S _ ds } = let
	(sb, sae, db, dae, cnt) = getCopyArgs c in
	M.Copy {
		M.copyNext = mnxt,
		M.copySrcSet = ss,
		M.copySrcBinding = sb,
		M.copySrcArrayElement = sae, M.copyDstSet = ds,
		M.copyDstBinding = db,
		M.copyDstArrayElement = dae, M.copyDescriptorCount = cnt }

copyToMiddleNew :: (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id, Copy.BindingLength bts ) =>
	CopyNew n sdss '(sls, btss) sdsd '(sld, btsd) bts is id -> M.Copy n
copyToMiddleNew c@CopyNew {
	copyNextNew = mnxt, copySrcSetNew = SNew _ ss, copyDstSetNew = SNew _ ds } = let
	(sb, sae, db, dae, cnt) = getCopyArgsNew c in
	M.Copy {
		M.copyNext = mnxt,
		M.copySrcSet = ss,
		M.copySrcBinding = sb,
		M.copySrcArrayElement = sae, M.copyDstSet = ds,
		M.copyDstBinding = db,
		M.copyDstArrayElement = dae, M.copyDescriptorCount = cnt }

getCopyArgs :: forall n sds sps sls btss sdd spd sld btsd bts is id . (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id,
	Copy.BindingLength bts ) =>
	Copy n sds sps '(sls, btss) sdd spd '(sld, btsd) bts is id ->
	(Word32, Word32, Word32, Word32, Word32)
getCopyArgs _ = let
	(sb, sae) = Copy.bindingAndArrayElement @btss @bts @is
	(db, dae) = Copy.bindingAndArrayElement @btsd @bts @id in
	(sb, sae, db, dae, Copy.bindingLength @bts)

getCopyArgsNew :: forall n sdss sls btss sdsd sld btsd bts is id . (
	Copy.BindingAndArrayElement btss bts is,
	Copy.BindingAndArrayElement btsd bts id,
	Copy.BindingLength bts ) =>
	CopyNew n sdss '(sls, btss) sdsd '(sld, btsd) bts is id ->
	(Word32, Word32, Word32, Word32, Word32)
getCopyArgsNew _ = let
	(sb, sae) = Copy.bindingAndArrayElement @btss @bts @is
	(db, dae) = Copy.bindingAndArrayElement @btsd @bts @id in
	(sb, sae, db, dae, Copy.bindingLength @bts)

deriving instance (
	Show (TMaybe.M n), Show (S sd sp slbts),
	Show (HeteroParList.PL Descriptor.BufferInfo sbsmobjsobjs)) =>
	Show (Write n sd sp slbts ('WriteSourcesArgBuffer sbsmobjsobjs))

writeUpdateLength :: forall sbsmobjsobjs n sd sp sl bts . (
	WriteSourcesToLengthList sbsmobjsobjs,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj sbsmobjsobjs) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj sbsmobjsobjs)
	) =>
	Write n sd sp '(sl, bts) sbsmobjsobjs -> IO ()
writeUpdateLength Write {
	writeDstSet = S rlns _,
	writeSources = ws } = do
	lns <- readIORef rlns
	maybe	(pure ())
		(writeIORef rlns . updateDynamicLength @bts @(WriteSourcesToLengthListObj sbsmobjsobjs) @0 lns
			. (VObj.onlyDynamicLength @(WriteSourcesToLengthListObj sbsmobjsobjs)))
		(writeSourcesToLengthList @sbsmobjsobjs ws)

writeUpdateLengthNew :: forall sbsmobjsobjs n s sl bts . (
	WriteSourcesToLengthList sbsmobjsobjs,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj sbsmobjsobjs) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj sbsmobjsobjs)
	) =>
	WriteNew n s '(sl, bts) sbsmobjsobjs -> IO ()
writeUpdateLengthNew WriteNew {
	writeDstSetNew = SNew rlns _,
	writeSourcesNew = ws } = do
	lns <- readIORef rlns
	maybe	(pure ())
		(writeIORef rlns . updateDynamicLength @bts @(WriteSourcesToLengthListObj sbsmobjsobjs) @0 lns
			. (VObj.onlyDynamicLength @(WriteSourcesToLengthListObj sbsmobjsobjs)))
		(writeSourcesToLengthList @sbsmobjsobjs ws)

class WriteListToMiddleNew nsdspslbtswsas where
	type WriteNexts nsdspslbtswsas :: [Maybe Type]
	writeListToMiddleNew ::
		HeteroParList.PL (U5 Write) nsdspslbtswsas ->
		HeteroParList.PL M.Write (WriteNexts nsdspslbtswsas)
	writeListUpdateLengthNew ::
		HeteroParList.PL (U5 Write) nsdspslbtswsas -> IO ()

instance WriteListToMiddleNew '[] where
	type WriteNexts '[] = '[]
	writeListToMiddleNew HeteroParList.Nil = HeteroParList.Nil
	writeListUpdateLengthNew HeteroParList.Nil = pure ()

instance (
	WriteSourcesToMiddle '(sl, bts) wsa,
	WriteListToMiddleNew nsdspslbtswsas,
	WriteSourcesToLengthList wsa,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj wsa) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj wsa) ) =>
	WriteListToMiddleNew ('(n, sd, sp, '(sl, bts), wsa) ': nsdspslbtswsas) where
	type WriteNexts ('(n, sd, sp, '(sl, bts), wsa) ': nsdspslbtswsas) =
		n ': WriteNexts nsdspslbtswsas
	writeListToMiddleNew (U5 w :** ws) =
		writeToMiddle w :** writeListToMiddleNew ws
	writeListUpdateLengthNew (U5 w :** ws) =
		writeUpdateLength w >> writeListUpdateLengthNew ws

class WriteListToMiddleNewNew nsdspslbtswsas where
	type WriteNextsNew nsdspslbtswsas :: [Maybe Type]
	writeListToMiddleNewNew ::
		HeteroParList.PL (U4 WriteNew) nsdspslbtswsas ->
		HeteroParList.PL M.Write (WriteNextsNew nsdspslbtswsas)
	writeListUpdateLengthNewNew ::
		HeteroParList.PL (U4 WriteNew) nsdspslbtswsas -> IO ()

instance WriteListToMiddleNewNew '[] where
	type WriteNextsNew '[] = '[]
	writeListToMiddleNewNew HeteroParList.Nil = HeteroParList.Nil
	writeListUpdateLengthNewNew HeteroParList.Nil = pure ()

instance (
	WriteSourcesToMiddle '(sl, bts) wsa,
	WriteListToMiddleNewNew nsdspslbtswsas,
	WriteSourcesToLengthList wsa,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj wsa) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj wsa) ) =>
	WriteListToMiddleNewNew ('(n, s, '(sl, bts), wsa) ': nsdspslbtswsas) where
	type WriteNextsNew ('(n, s, '(sl, bts), wsa) ': nsdspslbtswsas) =
		n ': WriteNextsNew nsdspslbtswsas
	writeListToMiddleNewNew (U4 w :** ws) =
		writeToMiddleNew w :** writeListToMiddleNewNew ws
	writeListUpdateLengthNewNew (U4 w :** ws) =
		writeUpdateLengthNew w >> writeListUpdateLengthNewNew ws

writeToMiddle :: forall n sd sp slbts wsa . WriteSourcesToMiddle slbts wsa =>
	Write n sd sp slbts wsa -> M.Write n
writeToMiddle Write {
	writeNext = mnxt,
	writeDstSet = S _ ds,
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

writeToMiddleNew :: forall n s slbts wsa . WriteSourcesToMiddle slbts wsa =>
	WriteNew n s slbts wsa -> M.Write n
writeToMiddleNew WriteNew {
	writeNextNew = mnxt,
	writeDstSetNew = SNew _ ds,
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
	Descriptor.BufferInfoListToLength sbsmobjsobjs =>
	WriteSourcesToLengthList ('WriteSourcesArgBuffer sbsmobjsobjs) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgBuffer sbsmobjsobjs) =
		Descriptor.BufferInfoListToLengthObjs sbsmobjsobjs
	writeSourcesToLengthList (BufferInfos bis) =
		Just $ Descriptor.bufferInfoListToLength bis

instance WriteSourcesToLengthList ('WriteSourcesArgImage ssfmtnmsis) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgImage ssfmtnmsis) = '[]
	writeSourcesToLengthList (ImageInfos _bis) = Nothing

instance WriteSourcesToLengthList ('WriteSourcesArgBufferView foo) where
	type WriteSourcesToLengthListObj
		('WriteSourcesArgBufferView foo) = '[]
	writeSourcesToLengthList (TexelBufferViews _) = Nothing

instance WriteSourcesToLengthList 'WriteSourcesArgOther where
	type WriteSourcesToLengthListObj 'WriteSourcesArgOther = '[]
	writeSourcesToLengthList (WriteSourcesInNext _ _ _) = Nothing
	writeSourcesToLengthList (TexelBufferViewsOld _ _ _) = Nothing

-- writeSourcesToLengthList :: WriteSources 

deriving instance Show (HeteroParList.PL Descriptor.BufferInfo sbsmobjsobjs) =>
	Show (WriteSources ('WriteSourcesArgBuffer sbsmobjsobjs))

class WriteListToMiddle n sdspslbtssbsmobjsobjs where
	writeListToMiddle ::
		HeteroParList.PL (U4 (Write n)) sdspslbtssbsmobjsobjs ->
		[M.Write n]
	writeListUpdateLength ::
		HeteroParList.PL (U4 (Write n)) sdspslbtssbsmobjsobjs -> IO ()

instance WriteListToMiddle n '[] where
	writeListToMiddle HeteroParList.Nil = []
	writeListUpdateLength HeteroParList.Nil = pure ()

instance (
	WriteSourcesToMiddle '(sl, bts) wsa,
	WriteListToMiddle n sdspslbtswsas,

	WriteSourcesToLengthList wsa,
	BindingAndArrayElem bts (WriteSourcesToLengthListObj wsa) 0,
	VObj.OnlyDynamicLengths (WriteSourcesToLengthListObj wsa)
	) =>
	WriteListToMiddle n
		('(sd, sp, '(sl, bts), wsa) ': sdspslbtswsas) where
	writeListToMiddle (U4 w :** ws) =
		writeToMiddle w : writeListToMiddle ws
	writeListUpdateLength (U4 w :** ws) =
		writeUpdateLength w >> writeListUpdateLength ws

updateDsNew :: (
	WriteListToMiddleNew sdspslbtssbsmobjsobjs,
	M.WriteListToCore (WriteNexts sdspslbtssbsmobjsobjs),
	CopyListToMiddle copyArgs, M.CopyListToCore (CopyNexts copyArgs) ) =>
	Device.D sd ->
	HeteroParList.PL (U5 Write) sdspslbtssbsmobjsobjs ->
	HeteroParList.PL (U10 Copy) copyArgs  -> IO ()
updateDsNew (Device.D dvc) ws cs =
	writeListUpdateLengthNew ws >> M.updateDs dvc ws' cs'
	where ws' = writeListToMiddleNew ws; cs' = copyListToMiddle cs

updateDsNewNew :: (
	WriteListToMiddleNewNew sdspslbtssbsmobjsobjs,
	M.WriteListToCore (WriteNextsNew sdspslbtssbsmobjsobjs),
	CopyListToMiddleNew copyArgs,
	M.CopyListToCore (CopyNextsNew copyArgs) ) =>
	Device.D sd ->
	HeteroParList.PL (U4 WriteNew) sdspslbtssbsmobjsobjs ->
	HeteroParList.PL (U8 CopyNew) copyArgs  -> IO ()
--	HeteroParList.PL (U10 Copy) copyArgs  -> IO ()
updateDsNewNew (Device.D dvc) ws cs =
	writeListUpdateLengthNewNew ws >> M.updateDs dvc ws' cs'
	where ws' = writeListToMiddleNewNew ws; cs' = copyListToMiddleNew cs
