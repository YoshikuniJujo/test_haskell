{-# LANGUAGE PackageImports, ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Kind
import Data.TypeLevel.Tuple.Uncurry
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.TypeLevel.ParMaybe (nil)
import Data.Default
import Data.Bits
import Data.Bits.ToolsYj
import Data.Tuple.ToolsYj
import Data.List.Length
import Data.Vector.Storable qualified as V
import Data.HeteroParList (pattern (:*.), pattern (:**))
import Data.HeteroParList qualified as HPList
import Data.Word


import Language.SpirV.Shaderc.TH
import Language.SpirV.ShaderKind

import Gpu.Vulkan qualified as Vk
import Gpu.Vulkan.TypeEnum qualified as Vk.TEnum
import Gpu.Vulkan.Object qualified as Obj
import Gpu.Vulkan.Object.Base qualified as BObj
import Gpu.Vulkan.Instance qualified as Vk.Inst
import Gpu.Vulkan.PhysicalDevice qualified as Vk.Phd
import Gpu.Vulkan.Queue qualified as Vk.Q
import Gpu.Vulkan.QueueFamily qualified as Vk.QFam
import Gpu.Vulkan.Device qualified as Vk.Dvc
import Gpu.Vulkan.Memory qualified as Vk.Mm
import Gpu.Vulkan.Buffer qualified as Vk.Bffr
import Gpu.Vulkan.BufferView qualified as Vk.BffrVw
import Gpu.Vulkan.CommandPool qualified as Vk.CmdPl
import Gpu.Vulkan.CommandBuffer qualified as Vk.CBffr
import Gpu.Vulkan.Cmd qualified as Vk.Cmd

import Gpu.Vulkan.Pipeline qualified as Vk.Ppl
import Gpu.Vulkan.Pipeline.Compute qualified as Vk.Ppl.Cmpt
import Gpu.Vulkan.Pipeline.ShaderStage qualified as Vk.Ppl.ShaderSt
import Gpu.Vulkan.PipelineLayout qualified as Vk.PplLyt
import Gpu.Vulkan.PushConstant qualified as Vk.PushConstant
import Gpu.Vulkan.ShaderModule qualified as Vk.ShaderMod
import Gpu.Vulkan.Descriptor qualified as Vk.Dsc
import Gpu.Vulkan.DescriptorPool qualified as Vk.DscPl
import Gpu.Vulkan.DescriptorSet qualified as Vk.DscSt
import Gpu.Vulkan.DescriptorSetLayout qualified as Vk.DscStLyt

---------------------------------------------------------------------------

-- MAIN
-- PREPARE BUFFERS AND MEMORIES
-- CALC

---------------------------------------------------------------------------

-- MAIN

main :: IO ()
main = withDvc \pd d q cpl mgcx ->
	let	da = V.replicate mgcx 0
		db = V.replicate mgcx 0
		dc = V.replicate mgcx 0
		dd = V.fromList . take mgcx $ cycle [
			Pixel 123 321 111 333, Pixel 555 444 333 222,
			Pixel 999 888 777 666 ] in
	Vk.DscStLyt.create d dslinfo nil \dsl ->
	createBffr4Mm4 pd d dsl da db dc dd \dss
		(ma :: Mm sm1 sb1 nm1 W1) (mb :: Mm sm2 sb2 nm2 W2)
		(mc :: Mm sm3 sb3 nm3 W3) -> do
	calc @nm1 @nm2 @nm3 @W1 @W2 @W3
		@(MmBffrArg sb1 nm1 W1) @(MmBffrArg sb2 nm2 W2)
		@(MmBffrArg sb3 nm3 W3) d q cpl dsl dss mgcx
	mapTup3M_ (print . take 20)
		. appTup3 (unW1 <$>) (unW2 <$>) (unW3 <$>) =<< (,,)
			<$> Vk.Mm.read @nm1 @(OList W1) @0 @[W1] d ma zeroBits
			<*> Vk.Mm.read @nm2 @(OList W2) @0 @[W2] d mb zeroBits
			<*> Vk.Mm.read @nm3 @(OList W3) @0 @[W3] d mc zeroBits
	where
	dslinfo :: Vk.DscStLyt.CreateInfo 'Nothing (DscStLytArg W1 W2 W3)
	dslinfo = Vk.DscStLyt.CreateInfo {
		Vk.DscStLyt.createInfoNext = TMaybe.N,
		Vk.DscStLyt.createInfoFlags = zeroBits,
		Vk.DscStLyt.createInfoBindings = bdg0 :** bdg1 :** HPList.Nil }
	bdg0 = Vk.DscStLyt.BindingBuffer {
		Vk.DscStLyt.bindingBufferDescriptorType =
			Vk.Dsc.TypeStorageBuffer,
		Vk.DscStLyt.bindingBufferStageFlags = Vk.ShaderStageComputeBit }
	bdg1 = Vk.DscStLyt.BindingBufferView {
		Vk.DscStLyt.bindingBufferViewDescriptorType =
			Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DscStLyt.bindingBufferViewStageFlags =
			Vk.ShaderStageComputeBit }

newtype W1 = W1 { unW1 :: Word32 } deriving (Show, Storable, Num)
newtype W2 = W2 { unW2 :: Word32 } deriving (Show, Storable, Num)
newtype W3 = W3 { unW3 :: Word32 } deriving (Show, Storable, Num)

type DscStLytArg w1 w2 w3 = '[
	'Vk.DscStLyt.Buffer '[OList w1, OList w2, OList w3],
	'Vk.DscStLyt.BufferView '[ '("", Pixel)] ]
type Mm sm sb nm w = Vk.Mm.M sm (MmBffrArg sb nm w)
type Bffr sm sb nm w = Vk.Bffr.Binded sm sb nm '[OList w]
type MmBffrArg sb nm w = '[ '(sb, 'Vk.Mm.BufferArg nm '[OList w])]
type OList t = Obj.List 256 t ""

withDvc :: (forall sd scpl .
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	(forall c . Integral c => c) -> IO a) -> IO a
withDvc a = Vk.Inst.create instInfo nil \inst -> do
	pd <- head' <$> Vk.Phd.enumerate inst
	mgcx :. _ <- Vk.Phd.limitsMaxComputeWorkGroupCount
		. Vk.Phd.propertiesLimits <$> Vk.Phd.getProperties pd
	qfi <- fst . head' . filter (
			checkBits Vk.Q.ComputeBit .
			Vk.QFam.propertiesQueueFlags . snd )
		<$> Vk.Phd.getQueueFamilyProperties pd
	Vk.Dvc.create pd (dvcInfo qfi) nil \dv ->
		Vk.Dvc.getQueue dv qfi 0 >>= \q ->
		Vk.CmdPl.create dv (cpinfo qfi) nil \cpl ->
			a pd dv q cpl $ fromIntegral mgcx
	where
	cpinfo qfi = Vk.CmdPl.CreateInfo {
		Vk.CmdPl.createInfoNext = TMaybe.N,
		Vk.CmdPl.createInfoFlags = Vk.CmdPl.CreateResetCommandBufferBit,
		Vk.CmdPl.createInfoQueueFamilyIndex = qfi }

instInfo :: Vk.Inst.CreateInfo 'Nothing 'Nothing
instInfo = def {
	Vk.Inst.createInfoEnabledLayerNames = [Vk.layerKhronosValidation] }

dvcInfo :: Vk.QFam.Index -> Vk.Dvc.CreateInfo 'Nothing '[ 'Nothing]
dvcInfo qfi = Vk.Dvc.CreateInfo {
	Vk.Dvc.createInfoNext = TMaybe.N, Vk.Dvc.createInfoFlags = zeroBits,
	Vk.Dvc.createInfoQueueCreateInfos = HPList.Singleton qinfo,
	Vk.Dvc.createInfoEnabledLayerNames = [Vk.layerKhronosValidation],
	Vk.Dvc.createInfoEnabledExtensionNames = [],
	Vk.Dvc.createInfoEnabledFeatures = Nothing }
	where qinfo = Vk.Dvc.QueueCreateInfo {
		Vk.Dvc.queueCreateInfoNext = TMaybe.N,
		Vk.Dvc.queueCreateInfoFlags = zeroBits,
		Vk.Dvc.queueCreateInfoQueueFamilyIndex = qfi,
		Vk.Dvc.queueCreateInfoQueuePriorities = [0] }

-- PREPARE BUFFERS AND MEMORIES

createBffr4Mm4 :: forall nm1 nm2 nm3 w1 w2 w3 sd sdsl bts a . (
	Storable w1, Storable w2, Storable w3,
	Vk.DscSt.BindingAndArrayElemBufferView bts '[ '("", Pixel)] 0,
	Vk.DscSt.BindingAndArrayElemBuffer
		bts '[OList w1, OList w2, OList w3] 0,
	Vk.DscSt.UpdateDynamicLength bts '[OList w1, OList w2, OList w3],
	Default (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Phd.P -> Vk.Dvc.D sd -> Vk.DscStLyt.D sdsl bts ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector Pixel -> (
		forall sds sm1 sm2 sm3 sb1 sb2 sb3 .
		Vk.DscSt.D sds '(sdsl, bts) ->
		Mm sm1 sb1 nm1 w1 -> Mm sm2 sb2 nm2 w2 -> Mm sm3 sb3 nm3 w3 ->
		IO a ) -> IO a
createBffr4Mm4 pd dv dsl da db dc dd a =
	Vk.DscPl.create dv dscPlInfo nil \dpl ->
	Vk.DscSt.allocateDs dv (dscStInfo dpl dsl) \(HPList.Singleton dss) ->
	bffr4Mm4 pd dv da db dc dd \ba bb bc bd ma mb mc _md ->
	Vk.BffrVw.create dv (bffrVwInfo bd) nil \bv ->
	Vk.DscSt.updateDs dv (
		U5 (writeDscStBffr3 @w1 @w2 @w3 dss ba bb bc) :**
		U5 (writeDscStBffrVw dss bv) :** HPList.Nil ) HPList.Nil >>
	a dss ma mb mc

bffrVwInfo :: Vk.Bffr.Binded sm sb nm '[OList Pixel] ->
	Vk.BffrVw.CreateInfo 'Nothing Pixel "" '(sm, sb, nm, '[OList Pixel])
bffrVwInfo bd = Vk.BffrVw.CreateInfo {
	Vk.BffrVw.createInfoNext = TMaybe.N,
	Vk.BffrVw.createInfoFlags = zeroBits,
	Vk.BffrVw.createInfoBuffer = U4 bd }

dscPlInfo :: Vk.DscPl.CreateInfo 'Nothing
dscPlInfo = Vk.DscPl.CreateInfo {
	Vk.DscPl.createInfoNext = TMaybe.N,
	Vk.DscPl.createInfoFlags = Vk.DscPl.CreateFreeDescriptorSetBit,
	Vk.DscPl.createInfoMaxSets = 1,
	Vk.DscPl.createInfoPoolSizes = [plszsb, plszstb] }
	where
	plszsb = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageBuffer,
		Vk.DscPl.sizeDescriptorCount = 3 }
	plszstb = Vk.DscPl.Size {
		Vk.DscPl.sizeType = Vk.Dsc.TypeStorageTexelBuffer,
		Vk.DscPl.sizeDescriptorCount = 1 }

dscStInfo :: Vk.DscPl.P sp -> Vk.DscStLyt.D sl bts ->
	Vk.DscSt.AllocateInfo 'Nothing sp '[ '(sl, bts)]
dscStInfo dpl dsl = Vk.DscSt.AllocateInfo {
	Vk.DscSt.allocateInfoNext = TMaybe.N,
	Vk.DscSt.allocateInfoDescriptorPool = dpl,
	Vk.DscSt.allocateInfoSetLayouts = HPList.Singleton $ U2 dsl }

bffr4Mm4 :: (Storable w1, Storable w2, Storable w3, Storable w4) =>
	Vk.Phd.P -> Vk.Dvc.D sd ->
	V.Vector w1 -> V.Vector w2 -> V.Vector w3 -> V.Vector w4 -> (
		forall sm1 sm2 sm3 sm4 sb1 sb2 sb3 sb4 .
		Vk.Bffr.Binded sm1 sb1 nm1 '[OList w1] ->
		Vk.Bffr.Binded sm2 sb2 nm2 '[OList w2] ->
		Vk.Bffr.Binded sm3 sb3 nm3 '[OList w3] ->
		Vk.Bffr.Binded sm4 sb4 nm4 '[OList w4] ->
		Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[OList w1])] ->
		Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[OList w2])] ->
		Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[OList w3])] ->
		Vk.Mm.M sm4 '[ '(sb4, 'Vk.Mm.BufferArg nm4 '[OList w4])] ->
		IO a ) -> IO a
bffr4Mm4 pd dv x y z w f =
	bffrMms pd dv (x :** y :** z :** w :** HPList.Nil) $ arg4 f

arg4 :: (forall sb1 sm1 sb2 sm2 sb3 sm3 sb4 sm4 .
	Vk.Bffr.Binded sm1 sb1 nm1 '[OList w1] ->
	Vk.Bffr.Binded sm2 sb2 nm2 '[OList w2] ->
	Vk.Bffr.Binded sm3 sb3 nm3 '[OList w3] ->
	Vk.Bffr.Binded sm4 sb4 nm4 '[OList w4] ->
	Vk.Mm.M sm1 '[ '(sb1, 'Vk.Mm.BufferArg nm1 '[OList w1])] ->
	Vk.Mm.M sm2 '[ '(sb2, 'Vk.Mm.BufferArg nm2 '[OList w2])] ->
	Vk.Mm.M sm3 '[ '(sb3, 'Vk.Mm.BufferArg nm3 '[OList w3])] ->
	Vk.Mm.M sm4 '[ '(sb4, 'Vk.Mm.BufferArg nm4 '[OList w4])] -> r) ->
	Arg nm1 w1 (Arg nm2 w2 (Arg nm3 w3 (Arg nm4 w4 r)))
arg4 f = Arg \b1 m1 ->
	Arg \b2 m2 -> Arg \b3 m3 -> Arg \b4 m4 -> f b1 b2 b3 b4 m1 m2 m3 m4

class BffrMms f a where
	type Vectors f :: [Type]
	bffrMms :: Vk.Phd.P ->
		Vk.Dvc.D sd -> HPList.PL V.Vector (Vectors f) -> f -> IO a

instance BffrMms (IO a) a where
	type Vectors (IO a) = '[]; bffrMms _pd _dv HPList.Nil f = f

instance (Storable w, BffrMms f a) => BffrMms (Arg nm w f) a where
	type Vectors (Arg nm w f) = w ': Vectors f
	bffrMms pd dv (vs :** vss) (Arg f) =
		bffrMm pd dv vs \b m -> bffrMms @f @a pd dv vss $ f b m

data Arg nm w f = Arg (forall sm sb .
	Vk.Bffr.Binded sm sb nm '[OList w] ->
	Vk.Mm.M sm '[ '(sb, 'Vk.Mm.BufferArg nm '[OList w])] -> f)

bffrMm :: forall sd nm w a . Storable w =>
	Vk.Phd.P -> Vk.Dvc.D sd -> V.Vector w ->
	(forall sm sb . Bffr sm sb nm w -> Mm sm sb nm w -> IO a) -> IO a
bffrMm pd dv xs a = Vk.Bffr.create dv (bffrInfo xs) nil \b ->
	mmInfo pd dv b >>= \mmi ->
	Vk.Mm.allocateBind dv (HPList.Singleton . U2 $ Vk.Mm.Buffer b) mmi nil
		\(HPList.Singleton (U2 (Vk.Mm.BufferBinded bnd))) mm ->
	Vk.Mm.write @nm @(OList w) @0 dv mm zeroBits xs >> a bnd mm

bffrInfo :: Storable w => V.Vector w -> Vk.Bffr.CreateInfo 'Nothing '[OList w]
bffrInfo xs = Vk.Bffr.CreateInfo {
	Vk.Bffr.createInfoNext = TMaybe.N,
	Vk.Bffr.createInfoFlags = zeroBits,
	Vk.Bffr.createInfoLengths =
		Obj.LengthList (fromIntegral $ V.length xs) :** HPList.Nil,
	Vk.Bffr.createInfoUsage =
		Vk.Bffr.UsageStorageBufferBit .|.
		Vk.Bffr.UsageStorageTexelBufferBit,
	Vk.Bffr.createInfoSharingMode = Vk.SharingModeExclusive,
	Vk.Bffr.createInfoQueueFamilyIndices = [] }

mmInfo :: Vk.Phd.P -> Vk.Dvc.D sd ->
	Vk.Bffr.B sb nm objs -> IO (Vk.Mm.AllocateInfo 'Nothing)
mmInfo pd dv bf = do
	rqs <- Vk.Bffr.getMemoryRequirements dv bf
	mti <- findMmTpIdx pd rqs
		$ Vk.Mm.PropertyHostVisibleBit .|. Vk.Mm.PropertyHostCoherentBit
	pure Vk.Mm.AllocateInfo {
		Vk.Mm.allocateInfoNext = TMaybe.N,
		Vk.Mm.allocateInfoMemoryTypeIndex = mti }

findMmTpIdx :: Vk.Phd.P ->
	Vk.Mm.Requirements -> Vk.Mm.PropertyFlags -> IO Vk.Mm.TypeIndex
findMmTpIdx pd rqs wt = Vk.Phd.getMemoryProperties pd >>= \prs ->
	let	rqts = Vk.Mm.requirementsMemoryTypeBits rqs
		wtts = (fst <$>)
			. filter (checkBits wt . Vk.Mm.mTypePropertyFlags . snd)
			$ Vk.Phd.memoryPropertiesMemoryTypes prs in
	case filter (`Vk.Mm.elemTypeIndex` rqts) wtts of
		[] -> error "No available memory types"; i : _ -> pure i

writeDscStBffr3 :: forall w1 w2 w3
	sds slbts sm1 sm2 sm3 sb1 sb2 sb3 nm1 nm2 nm3 os1 os2 os3 . (
	Show (HPList.PL Obj.Length os1), Show (HPList.PL Obj.Length os2),
	Show (HPList.PL Obj.Length os3),
	Obj.OffsetRange (OList w1) os1 0, Obj.OffsetRange (OList w2) os2 0,
	Obj.OffsetRange (OList w3) os3 0 ) =>
	Vk.DscSt.D sds slbts ->
	Vk.Bffr.Binded sm1 sb1 nm1 os1 -> Vk.Bffr.Binded sm2 sb2 nm2 os2 ->
	Vk.Bffr.Binded sm3 sb3 nm3 os3 ->
	Vk.DscSt.Write 'Nothing sds slbts ('Vk.DscSt.WriteSourcesArgBuffer '[
		'(sm1, sb1, nm1, OList w1, 0), '(sm2, sb2, nm2, OList w2, 0),
		'(sm3, sb3, nm3, OList w3, 0) ]) 0
writeDscStBffr3 ds ba bb bc = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = ds,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageBuffer,
	Vk.DscSt.writeSources = Vk.DscSt.BufferInfos $
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList w1) ba) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList w2) bb) :**
		U5 (Vk.Dsc.BufferInfo @_ @_ @_ @(OList w3) bc) :** HPList.Nil }

writeDscStBffrVw :: Vk.DscSt.D sds sdslbts -> Vk.BffrVw.B sbv bvnm fmt ->
	Vk.DscSt.Write 'Nothing sds sdslbts
		(Vk.DscSt.WriteSourcesArgBufferView '[ '(sbv, bvnm, fmt)]) 0
writeDscStBffrVw dss bv = Vk.DscSt.Write {
	Vk.DscSt.writeNext = TMaybe.N, Vk.DscSt.writeDstSet = dss,
	Vk.DscSt.writeDescriptorType = Vk.Dsc.TypeStorageTexelBuffer,
	Vk.DscSt.writeSources =
		Vk.DscSt.TexelBufferViews . HPList.Singleton $ U3 bv }

-- CALC

calc :: forall nm1 nm2 nm3 w1 w2 w3 oss1 oss2 oss3
	sd scpl sds slbts sl bts . (
	Storable w1, Storable w2, Storable w3,
	slbts ~ '(sl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	Vk.Mm.OffsetSize nm1 (OList w1) oss1 0,
	Vk.Mm.OffsetSize nm2 (OList w2) oss2 0,
	Vk.Mm.OffsetSize nm3 (OList w3) oss3 0,
	Show (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Dvc.D sd -> Vk.Q.Q -> Vk.CmdPl.C scpl ->
	Vk.DscStLyt.D sl bts -> Vk.DscSt.D sds slbts -> Word32 -> IO ()
calc dv q cpl dsl dss sz =
	Vk.PplLyt.create dv (pplLytInfo dsl) nil \pl ->
	Vk.Ppl.Cmpt.createCs dv Nothing (HPList.Singleton . U4 $ cmpPplInfo pl)
		nil \(cppl :** HPList.Nil) ->
	Vk.CBffr.allocateCs dv (cmdBffrInfo cpl) \(cb :*. HPList.Nil) ->
	run @nm1 @nm2 @nm3 @w1 @w2 @w3 @oss1 @oss2 @oss3 q cb pl cppl dss sz

pplLytInfo :: Vk.DscStLyt.D sl bts -> Vk.PplLyt.CreateInfo
	'Nothing '[ '(sl, bts)] ('Vk.PushConstant.Layout '[] '[])
pplLytInfo dsl = Vk.PplLyt.CreateInfo {
	Vk.PplLyt.createInfoNext = TMaybe.N,
	Vk.PplLyt.createInfoFlags = zeroBits,
	Vk.PplLyt.createInfoSetLayouts = HPList.Singleton $ U2 dsl }

cmpPplInfo :: Vk.PplLyt.P sl sbtss '[] -> Vk.Ppl.Cmpt.CreateInfo 'Nothing
	'( 'Nothing, 'Nothing, 'GlslComputeShader, 'Nothing, '[])
	'(sl, sbtss, '[]) sbph
cmpPplInfo pl = Vk.Ppl.Cmpt.CreateInfo {
	Vk.Ppl.Cmpt.createInfoNext = TMaybe.N,
	Vk.Ppl.Cmpt.createInfoFlags = zeroBits,
	Vk.Ppl.Cmpt.createInfoStage = U5 shdrStInfo,
	Vk.Ppl.Cmpt.createInfoLayout = U3 pl,
	Vk.Ppl.Cmpt.createInfoBasePipelineHandleOrIndex = Nothing }

shdrStInfo :: Vk.Ppl.ShaderSt.CreateInfo
	'Nothing 'Nothing 'GlslComputeShader 'Nothing '[]
shdrStInfo = Vk.Ppl.ShaderSt.CreateInfo {
	Vk.Ppl.ShaderSt.createInfoNext = TMaybe.N,
	Vk.Ppl.ShaderSt.createInfoFlags = zeroBits,
	Vk.Ppl.ShaderSt.createInfoStage = Vk.ShaderStageComputeBit,
	Vk.Ppl.ShaderSt.createInfoModule = (mdinfo, nil),
	Vk.Ppl.ShaderSt.createInfoName = "main",
	Vk.Ppl.ShaderSt.createInfoSpecializationInfo = Nothing }
	where mdinfo = Vk.ShaderMod.CreateInfo {
		Vk.ShaderMod.createInfoNext = TMaybe.N,
		Vk.ShaderMod.createInfoFlags = zeroBits,
		Vk.ShaderMod.createInfoCode = glslComputeShaderMain }

cmdBffrInfo :: Vk.CmdPl.C s -> Vk.CBffr.AllocateInfo 'Nothing s '[ '()]
cmdBffrInfo cpl = Vk.CBffr.AllocateInfo {
	Vk.CBffr.allocateInfoNext = TMaybe.N,
	Vk.CBffr.allocateInfoCommandPool = cpl,
	Vk.CBffr.allocateInfoLevel = Vk.CBffr.LevelPrimary }

run :: forall nm1 nm2 nm3 w1 w2 w3 oss1 oss2 oss3
	sc sg spl sds slbts sdsl bts . (
	Storable w1, Storable w2, Storable w3,
	Vk.Mm.OffsetSize nm1 (OList w1) oss1 0,
	Vk.Mm.OffsetSize nm2 (OList w2) oss2 0,
	Vk.Mm.OffsetSize nm3 (OList w3) oss3 0,
	slbts ~ '(sdsl, bts),
	Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts ~ '[ '[], '[]],
	Show (HPList.PL2 BObj.Length
		(Vk.DscStLyt.BindingTypeListBufferOnlyDynamics bts)) ) =>
	Vk.Q.Q -> Vk.CBffr.C sc -> Vk.PplLyt.P spl '[slbts] '[] ->
	Vk.Ppl.Cmpt.C sg '(spl, '[slbts], '[]) ->
	Vk.DscSt.D sds slbts -> Word32 -> IO ()
run q cb pl cppl dss sz = do
	Vk.CBffr.begin @'Nothing @'Nothing cb def $
		Vk.Cmd.bindPipelineCompute
			cb Vk.Ppl.BindPointCompute cppl \ccb ->
		Vk.Cmd.bindDescriptorSetsCompute
			ccb pl (HPList.Singleton $ U2 dss)
			(HPList.Singleton
				$ HPList.Nil :** HPList.Nil :** HPList.Nil) >>
		Vk.Cmd.dispatch ccb sz 1 1
	Vk.Q.submit q (HPList.Singleton $ U4 sinfo) Nothing
	Vk.Q.waitIdle q
	where sinfo = Vk.SubmitInfo {
		Vk.submitInfoNext = TMaybe.N,
		Vk.submitInfoWaitSemaphoreDstStageMasks = HPList.Nil,
		Vk.submitInfoCommandBuffers = HPList.Singleton cb,
		Vk.submitInfoSignalSemaphores = HPList.Nil }

data Pixel = Pixel Word32 Word32 Word32 Word32 deriving Show
type instance Vk.BffrVw.FormatOf Pixel = 'Vk.TEnum.FormatR32g32b32a32Uint

instance Storable Pixel where
	sizeOf _ = 4 * sizeOf @Word32 undefined
	alignment _ = alignment @Word32 undefined
	peek p = do
		[r, g, b, a] <- peekArray 4 $ castPtr p
		pure $ Pixel r g b a
	poke p (Pixel r g b a) = pokeArray (castPtr p) [r, g, b, a]

head' :: [a] -> a
head' = \case [] -> error "empty list"; x : _ -> x

[glslComputeShader|

#version 460

layout(local_size_x = 1, local_size_y = 1) in;
layout(binding = 0) buffer Data { uint val[]; } data[3];
layout(binding = 1, rgba32ui) uniform uimageBuffer txb;

void
main()
{
	int i = int(gl_GlobalInvocationID.x);
	uvec4 px = imageLoad(txb, i);
	data[0].val[i] = px.x; data[1].val[i] = px.y; data[2].val[i] = px.z;
}

|]
