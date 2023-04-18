{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
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
import Data.TypeLevel.Uncurry
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
	allocateInfoNext :: Maybe n,
	allocateInfoDescriptorPool :: Descriptor.Pool.P sp,
	allocateInfoSetLayouts :: HeteroParList.PL Layout slbtss }

deriving instance (Show n, Show (HeteroParList.PL Layout slbtss)) =>
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

allocateSs :: (WithPoked n, SListFromMiddle slbtss) =>
	Device.D sd -> AllocateInfo n sp slbtss ->
	IO (HeteroParList.PL (S sd sp) slbtss)
allocateSs (Device.D dvc) ai =
	sListFromMiddle =<< M.allocateDs dvc (allocateInfoToMiddle ai)

data Write n sd sp (slbts :: LayoutArg)
	(sbsmobjsobjs :: WriteSourcesArg) = Write {
	writeNext :: Maybe n,
	writeDstSet :: S sd sp slbts,
	writeDescriptorType :: Descriptor.Type,
	writeSources :: WriteSources sbsmobjsobjs }

data Copy n sds sps (slbtss :: LayoutArg) sdd spd (slbtsd :: LayoutArg)
	(bts :: Layout.BindingType) (is :: Nat) (id :: Nat) = Copy {
	copyNext :: Maybe n,
	copySrcSet :: S sds sps slbtss,
	copyDstSet :: S sdd spd slbtsd }

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

deriving instance (
	Show n, Show (S sd sp slbts),
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

class WriteListToMiddleNew nsdspslbtswsas where
	type WriteNexts nsdspslbtswsas :: [Type]
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

updateDs :: (
	WithPoked n, WithPoked n',
	WriteListToMiddle n sdspslbtssbsmobjsobjs ) =>
	Device.D sd ->
	HeteroParList.PL (U4 (Write  n)) sdspslbtssbsmobjsobjs -> [M.Copy n'] -> IO ()
updateDs (Device.D dvc) ws cs =
	writeListUpdateLength ws >>
	M.updateDs dvc ws' cs
	where ws' = writeListToMiddle ws

updateDsNew :: (
	WithPoked n, WithPoked n',
	WriteListToMiddleNew sdspslbtssbsmobjsobjs,
	M.WriteListToCore (WriteNexts sdspslbtssbsmobjsobjs) ) =>
	Device.D sd ->
	HeteroParList.PL (U5 Write) sdspslbtssbsmobjsobjs -> [M.Copy n'] -> IO ()
updateDsNew (Device.D dvc) ws cs =
	writeListUpdateLengthNew ws >>
	M.updateDsNew dvc ws' HeteroParList.Nil
	where ws' = writeListToMiddleNew ws
